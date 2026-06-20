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
  (:import (java.io File)
           (javafx.application Platform)
           (javafx.stage DirectoryChooser)))

(defn- choose-directory!
  "Show a directory picker and return its file URI string, or nil if cancelled.
  Must run on the JavaFX Application Thread."
  [title]
  (let [chooser (doto (DirectoryChooser.) (.setTitle ^String title))]
    (when-let [^File dir (.showDialog chooser nil)]
      (-> dir (.toURI) (.toString)))))

(defn- persist! [state-atom]
  (let [{:keys [store-path libraries]} @state-atom]
    (when store-path (store/save! store-path libraries))))

(defn- reload-catalogs!
  "Re-scan the selected source and sink libraries and refresh catalogs +
  capacity. Pre-selects tracks already on the sink (via state/set-catalogs)."
  [state-atom]
  (let [s   @state-atom
        src (state/library-by-id s (:source-id s))
        snk (state/library-by-id s (:sink-id s))]
    (when (and src snk)
      (swap! state-atom (fn [s] (-> s
                                    (state/set-status :scanning)
                                    (state/append-log (format "Scanning '%s' → '%s'…"
                                                              (:name src) (:name snk))))))
      (try
        (let [src-cat (sync/catalog-of! src)
              snk-cat (sync/catalog-of! snk)
              free    (sync/library-free! snk)]
          (swap! state-atom
                 (fn [s] (-> s
                             (state/set-catalogs src-cat snk-cat free)
                             (state/set-status :idle)
                             (state/append-log (format "Source %d · Sink %d tracks · %s free."
                                                       (count src-cat) (count snk-cat)
                                                       (fmt/human-bytes free)))))))
        (catch Throwable t
          (swap! state-atom (fn [s] (-> s
                                        (state/set-error (.getMessage t))
                                        (state/append-log (str "Scan failed: " (.getMessage t)))))))))))

(defn- run-preview! [state-atom]
  (let [s   @state-atom
        src (state/library-by-id s (:source-id s))
        snk (state/library-by-id s (:sink-id s))]
    (swap! state-atom (fn [s] (-> s
                                  (state/set-status :scanning)
                                  (state/append-log "Computing plan…"))))
    (try
      (let [actions (sync/build-plan! src snk (:selected s))
            summ    (plan/summary actions)]
        (swap! state-atom (fn [s] (-> s
                                      (state/set-plan actions summ)
                                      (state/append-log (fmt/plan-summary-text summ))))))
      (catch Throwable t
        (swap! state-atom (fn [s] (-> s
                                      (state/set-error (.getMessage t))
                                      (state/append-log (str "Plan failed: " (.getMessage t))))))))))

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

(defn- detect-mtp-storages!
  "Enumerate connected MTP devices and their storages into editor candidates."
  [state-atom]
  (swap! state-atom (fn [s] (state/append-log s "Detecting MTP storages…")))
  (try
    (let [devs       ((requiring-resolve 'dapr.fs.mtp/devices!))
          candidates (vec (for [d       devs
                                storage (try (nio/children! (:uri d)) (catch Throwable _ []))]
                            {:uri   (str (:uri d) storage)
                             :label (str (:name d) " / " storage)}))]
      (swap! state-atom (fn [s] (-> s
                                    (state/set-devices devs)
                                    (state/set-mtp-candidates candidates)
                                    (state/append-log (format "Found %d MTP storage(s)."
                                                              (count candidates)))))))
    (catch Throwable t
      (swap! state-atom (fn [s] (state/append-log s (str "MTP unavailable: " (.getMessage t))))))))

(defn- library-id-by-name [state-atom nm]
  (:id (first (filter #(= nm (:name %)) (:libraries @state-atom)))))

(defn make-handler
  "Return a cljfx event handler closing over `state-atom`."
  [state-atom]
  (fn [event]
    (case (:event/type event)
      ;; library manager
      ::library-new    (swap! state-atom state/set-editor
                              {:id (str (random-uuid)) :name "" :roots [] :pending-uri ""})
      ::library-edit   (when-let [l (state/library-by-id @state-atom (:id event))]
                         (swap! state-atom state/set-editor (assoc l :pending-uri "")))
      ::library-delete (do (swap! state-atom state/delete-library (:id event))
                           (persist! state-atom))

      ;; editor
      ::editor-name        (swap! state-atom state/editor-name (:fx/event event))
      ::editor-pending-uri (swap! state-atom state/editor-pending-uri (:fx/event event))
      ::editor-add-pending (swap! state-atom state/editor-add-pending)
      ::editor-browse      (when-let [uri (choose-directory! "Add folder to library")]
                             (swap! state-atom state/editor-add-root uri))
      ::editor-remove-root (swap! state-atom state/editor-remove-root (:uri event))
      ::editor-detect-mtp  (future (detect-mtp-storages! state-atom))
      ::editor-add-candidate
      (let [nm  (:fx/event event)
            uri (:uri (first (filter #(= nm (:label %)) (:mtp-candidates @state-atom))))]
        (when uri (swap! state-atom state/editor-add-root uri)))
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
