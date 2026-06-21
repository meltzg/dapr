(ns dapr.ui.events
  "Side-effecting event handlers for the cljfx UI. Pure state transitions live in
  dapr.state; filesystem work lives in dapr.fs.nio / dapr.sync; persistence in
  dapr.library.store. Each handler runs on the JavaFX Application Thread, so
  long-running scans/copies are dispatched to background threads to keep the UI
  responsive."
  (:require [dapr.domain.library :as lib]
            [dapr.domain.plan :as plan]
            [dapr.fs.nio :as nio]
            [dapr.library.store :as store]
            [dapr.state :as state]
            [dapr.sync :as sync]
            [dapr.ui.format :as fmt])
  (:import (javafx.application Platform)))

(defn- persist! [state-atom]
  (let [{:keys [store-path libraries]} @state-atom]
    (when store-path (store/save! store-path libraries))))

(defn- scan-logger
  "Scan-event callback that appends progress to the activity log, tagged with the
  library being scanned. Logs each directory as it is entered (the last such line
  before a freeze pinpoints the directory whose listing hung) and each audio file
  with a running count of tracks scanned so far in that library."
  [state-atom label]
  (let [scanned (atom 0)]
    (fn [{:keys [type rel track]}]
      (case type
        :dir  (swap! state-atom state/append-log
                     (format "  [%s] scanning %s/" label (if (= "" rel) "" rel)))
        :file (let [n (swap! scanned inc)]
                (swap! state-atom state/append-log
                       (format "  [%s] #%d %s" label n (or (:rel track) (:name track)))))
        nil))))

(defn- begin-scan!
  "Start a new scan generation, superseding any in-flight one. Returns a
  `superseded?` predicate that becomes true once a later scan begins."
  [state-atom]
  (let [gen (:scan-gen (swap! state-atom update :scan-gen inc))]
    (fn [] (not= gen (:scan-gen @state-atom)))))

(defn- superseded-ex?
  "True when `t` (or any of its causes) is the marker thrown to abort a scan that
  a newer one has superseded."
  [t]
  (boolean (some #(:dapr/abort (ex-data %))
                 (take-while some? (iterate #(some-> ^Throwable % .getCause) t)))))

(defn- scan-callback
  "An on-scan callback that logs progress (see scan-logger) but first aborts the
  walk, by throwing, once `superseded?` reports a newer scan has started."
  [state-atom label superseded?]
  (let [log (scan-logger state-atom label)]
    (fn [ev]
      (when (superseded?)
        (throw (ex-info "scan superseded" {:dapr/abort true})))
      (log ev))))

(defn- reload-catalogs!
  "Re-scan the selected source and sink libraries and refresh catalogs +
  capacity. Pre-selects tracks already on the sink (via state/set-catalogs).
  Supersedes any scan still running from an earlier source/sink change."
  [state-atom]
  (let [superseded? (begin-scan! state-atom)
        s           @state-atom
        src         (state/library-by-id s (:source-id s))
        snk         (state/library-by-id s (:sink-id s))]
    (when (and src snk)
      (swap! state-atom (fn [s] (-> s
                                    (state/set-status :scanning)
                                    (state/append-log (format "Scanning '%s' → '%s'…"
                                                              (:name src) (:name snk))))))
      (try
        ;; Scan source and sink concurrently — melt-jfs serializes per device, so
        ;; this overlaps work whenever they are on different devices (the common
        ;; case: one local, one MTP) and is harmless when they share one.
        (let [src-fut (future (sync/catalog-of! src (scan-callback state-atom (:name src) superseded?)))
              snk-cat (sync/catalog-of! snk (scan-callback state-atom (:name snk) superseded?))
              src-cat @src-fut
              free    (sync/library-free! snk)]
          (when-not (superseded?)
            (swap! state-atom
                   (fn [s] (-> s
                               (state/set-catalogs src-cat snk-cat free)
                               (state/set-status :idle)
                               (state/append-log (format "Source %d · Sink %d tracks · %s free."
                                                         (count src-cat) (count snk-cat)
                                                         (fmt/human-bytes free))))))))
        (catch Throwable t
          (when-not (superseded-ex? t)
            (swap! state-atom (fn [s] (-> s
                                          (state/set-error (.getMessage t))
                                          (state/append-log (str "Scan failed: " (.getMessage t))))))))))))

(defn- run-preview! [state-atom]
  (let [superseded? (begin-scan! state-atom)
        s           @state-atom
        src         (state/library-by-id s (:source-id s))
        snk         (state/library-by-id s (:sink-id s))]
    (swap! state-atom (fn [s] (-> s
                                  (state/set-status :scanning)
                                  (state/append-log "Computing plan…"))))
    (try
      (let [actions (sync/build-plan! src snk (:selected s)
                                      {:on-source (scan-callback state-atom (:name src) superseded?)
                                       :on-sink   (scan-callback state-atom (:name snk) superseded?)})
            summ    (plan/summary actions)]
        (when-not (superseded?)
          (swap! state-atom (fn [s] (-> s
                                        (state/set-plan actions summ)
                                        (state/append-log (fmt/plan-summary-text summ)))))))
      (catch Throwable t
        (when-not (superseded-ex? t)
          (swap! state-atom (fn [s] (-> s
                                        (state/set-error (.getMessage t))
                                        (state/append-log (str "Plan failed: " (.getMessage t)))))))))))

(defn- run-sync! [state-atom]
  (let [actions (get-in @state-atom [:plan :actions])]
    (swap! state-atom (fn [s] (-> s
                                  (state/set-status :syncing)
                                  (state/append-log "Syncing…"))))
    (try
      (let [result (sync/execute-plan!
                    actions
                    {:on-progress (fn [p] (swap! state-atom state/set-progress
                                                 (select-keys p [:done :total])))})]
        (swap! state-atom (fn [s] (-> s
                                      (state/set-status :done)
                                      (state/set-progress nil)
                                      (state/append-log
                                       (format "Done. Added %d, deleted %d."
                                               (:add result) (:delete result))))))
        (reload-catalogs! state-atom))
      (catch Throwable t
        (swap! state-atom (fn [s] (-> s
                                      (state/set-error (.getMessage t))
                                      (state/append-log (str "Sync failed: " (.getMessage t))))))))))

(defn- load-devices!
  "Detect connected MTP devices on a background thread and store them as the
  browser's device list. MTP discovery is optional and lazy (the jar may be
  absent), so it degrades to an empty list on any failure."
  [state-atom]
  (future
    (let [devices (try (vec ((requiring-resolve 'dapr.fs.mtp/devices!)))
                       (catch Throwable _ []))]
      (swap! state-atom state/browser-set-devices devices))))

(defn- browse-load!
  "List the browser's current directory on a background thread and store the
  result as its entries. A nil :cwd (file:// only) lists the local places."
  [state-atom]
  (let [cwd (get-in @state-atom [:browser :cwd])]
    (future
      (try
        (let [entries (if cwd (nio/dir-children! cwd) (nio/local-places!))]
          (swap! state-atom state/browser-set-entries entries))
        (catch Throwable t
          (swap! state-atom (fn [s] (-> s
                                        (state/browser-set-entries [])
                                        (state/append-log (str "Browse failed: " (.getMessage t)))))))))))

(def ^:private mixed-device-msg
  "A library's roots must all live on one device — remove the existing roots first to switch device.")

(defn- library-id-by-name [state-atom nm]
  (:id (first (filter #(= nm (:name %)) (:libraries @state-atom)))))

(defn make-handler
  "Return a cljfx event handler closing over `state-atom`."
  [state-atom]
  (fn [event]
    (case (:event/type event)
      ;; settings modal
      ::settings-open  (swap! state-atom state/open-settings)
      ::settings-close (swap! state-atom state/close-settings)

      ;; library manager — the device type is chosen from the New… submenu and
      ;; pins the new library to file:// or mtp:// (editing derives it from the
      ;; existing roots)
      ::library-new    (swap! state-atom state/set-editor
                              {:id (str (random-uuid)) :name "" :roots []
                               :kind (:kind event)})
      ::library-edit   (when-let [l (state/library-by-id @state-atom (:id event))]
                         (swap! state-atom state/set-editor
                                (assoc l :kind (some-> (first (:roots l)) lib/scheme keyword))))
      ::library-delete (do (swap! state-atom state/delete-library (:id event))
                           (persist! state-atom))

      ;; editor
      ::editor-name        (swap! state-atom state/editor-name (:fx/event event))
      ::editor-remove-root (swap! state-atom state/editor-remove-root (:uri event))

      ;; folder browser — the editor's :kind decides where it opens: file://
      ;; navigates folders directly, mtp:// first picks one connected device
      ::editor-browse        (case (get-in @state-atom [:editor :kind])
                               :file (do (swap! state-atom state/browser-choose-file)
                                         (browse-load! state-atom))
                               :mtp  (do (swap! state-atom state/browser-choose-mtp)
                                         (load-devices! state-atom))
                               nil)
      ::browser-device       (do (swap! state-atom state/browser-choose-device (:device event))
                                 (browse-load! state-atom))
      ::browser-enter        (do (swap! state-atom state/browser-enter (:child event))
                                 (browse-load! state-atom))
      ::browser-crumb        (do (swap! state-atom state/browser-to-crumb (:idx event))
                                 (browse-load! state-atom))
      ::browser-places       (do (swap! state-atom state/browser-to-places)
                                 (browse-load! state-atom))
      ::browser-select (when-let [uri (get-in @state-atom [:browser :cwd])]
                         (if (lib/root-addable? (get-in @state-atom [:editor :roots]) uri)
                           (swap! state-atom (fn [s] (-> s
                                                         (state/editor-add-root uri)
                                                         (state/browser-close))))
                           (swap! state-atom state/append-log mixed-device-msg)))
      ::browser-cancel (swap! state-atom state/browser-close)

      ::editor-save
      (let [library (select-keys (:editor @state-atom) [:id :name :roots])]
        (if (lib/library-valid? library)
          (do (swap! state-atom (fn [s] (-> s (state/upsert-library library) (state/cancel-editor))))
              (persist! state-atom))
          (swap! state-atom state/append-log
                 "Library needs a name and at least one file:// or mtp:// root.")))
      ::editor-cancel (swap! state-atom state/cancel-editor)

      ;; sync workflow
      ::select-source (do (swap! state-atom state/select-source
                                 (library-id-by-name state-atom (:fx/event event)))
                          (future (reload-catalogs! state-atom)))
      ::select-sink   (do (swap! state-atom state/select-sink
                                 (library-id-by-name state-atom (:fx/event event)))
                          (future (reload-catalogs! state-atom)))
      ::toggle-track  (swap! state-atom state/toggle-track (:key event))
      ::preview       (future (run-preview! state-atom))
      ::sync          (future (run-sync! state-atom))
      ::quit          (do (Platform/exit) (System/exit 0))
      nil)))
