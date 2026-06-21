(ns dapr.state
  "Application state: a single map describing the configured libraries, the
  current source/sink selection, the chosen tracks, and sync status. The
  transition functions here are pure (state -> state); the atom that holds the
  state is created and managed by the Integrant system (dapr.system), and the
  side-effecting event handlers (dapr.ui.events) apply these transitions via
  swap! (and persist libraries to disk)."
  (:require [clojure.string :as str]
            [dapr.domain.capacity :as cap]
            [dapr.domain.library :as lib]))

(def initial-state
  "The state map a freshly started system begins with."
  {:libraries      []     ; persisted [{:id :name :roots}]
   :store-path     nil    ; where libraries are persisted
   :source-id      nil
   :sink-id        nil
   :source-catalog {}     ; key -> track
   :sink-catalog   {}     ; key -> track
   :selected       #{}    ; set of selected track keys
   :free-bytes     0      ; usable bytes across the sink's distinct devices
   :capacity       {:used 0 :budget 0 :free 0}
   :plan           nil    ; {:actions [...] :summary {...}}
   :settings-open? false  ; whether the library-management modal is showing
   :editor         nil    ; library being added/edited, or nil
   :browser        nil    ; folder browser, or nil — see browser-open
   :status         :idle  ; :idle :scanning :planned :syncing :done :error
   :scan-gen       0      ; bumped per scan; lets a new scan supersede a running one
   :progress       nil    ; {:done n :total t}
   :log            []     ; vector of message strings (capped at max-log-lines)
   :log-appends    0      ; total lines ever appended; drives log auto-scroll
   :error          nil})

(def max-log-lines
  "Upper bound on retained activity-log lines; older lines are dropped."
  500)

;; --- libraries ---------------------------------------------------------------

(defn set-libraries [state libraries] (assoc state :libraries (vec libraries)))

(defn library-by-id [state id] (first (filter #(= (:id %) id) (:libraries state))))

(defn upsert-library
  "Insert `library` or replace the existing one with the same :id."
  [state {:keys [id] :as library}]
  (let [libs (vec (:libraries state))
        idx  (first (keep-indexed (fn [i l] (when (= (:id l) id) i)) libs))]
    (assoc state :libraries (if idx (assoc libs idx library) (conj libs library)))))

(defn delete-library
  "Remove the library with `id`, clearing it from source/sink if selected."
  [state id]
  (-> state
      (update :libraries (fn [libs] (vec (remove #(= (:id %) id) libs))))
      (cond-> (= id (:source-id state)) (assoc :source-id nil)
              (= id (:sink-id state))   (assoc :sink-id nil))))

;; --- source / sink / selection ----------------------------------------------

(defn- recompute-capacity [{:keys [selected source-catalog sink-catalog free-bytes] :as state}]
  (assoc state :capacity (cap/usage selected source-catalog sink-catalog free-bytes)))

(defn select-source [state id] (assoc state :source-id id))
(defn select-sink [state id] (assoc state :sink-id id))

(defn set-catalogs
  "Record freshly scanned catalogs and free space, pre-select the tracks already
  on the sink, and recompute capacity."
  [state source-catalog sink-catalog free-bytes]
  (-> state
      (assoc :source-catalog source-catalog
             :sink-catalog sink-catalog
             :free-bytes free-bytes
             :selected (lib/initial-selection sink-catalog))
      (recompute-capacity)))

(defn toggle-track
  "Toggle selection of track `k`. Selecting is refused (no-op) when it would
  exceed the sink's capacity. Deselecting always succeeds."
  [{:keys [selected source-catalog sink-catalog free-bytes] :as state} k]
  (cond
    (contains? selected k)
    (-> state (update :selected disj k) (recompute-capacity))

    (cap/would-fit? k selected source-catalog sink-catalog free-bytes)
    (-> state (update :selected conj k) (recompute-capacity))

    :else state))

;; --- settings modal ----------------------------------------------------------

(defn open-settings [state] (assoc state :settings-open? true))

(defn close-settings
  "Hide the settings modal, discarding any in-progress editor/browser."
  [state]
  (assoc state :settings-open? false :editor nil :browser nil))

;; --- editor ------------------------------------------------------------------

(defn set-editor [state editor] (assoc state :editor editor))
(defn cancel-editor [state] (assoc state :editor nil))
(defn editor-name [state name] (assoc-in state [:editor :name] name))
(defn editor-pending-uri [state uri] (assoc-in state [:editor :pending-uri] uri))

(defn editor-add-root
  "Append `uri` to the library being edited (ignoring blanks/duplicates)."
  [state uri]
  (if (str/blank? uri)
    state
    (update-in state [:editor :roots]
               (fn [roots] (if (some #{uri} roots) roots (conj (vec roots) uri))))))

(defn editor-add-pending
  "Append the editor's pending URI to its roots and clear the pending field."
  [state]
  (-> state
      (editor-add-root (get-in state [:editor :pending-uri]))
      (assoc-in [:editor :pending-uri] "")))

(defn editor-remove-root
  [state uri]
  (update-in state [:editor :roots] (fn [roots] (vec (remove #(= % uri) roots)))))

;; --- folder browser ----------------------------------------------------------
;; A single list+breadcrumb browser used for both file:// and mtp:// roots. Its
;; :cwd is the directory currently shown (nil = the top-level "places" list);
;; :crumbs is the trail of {:label :uri} from the chosen place down to :cwd;
;; :entries is the list of child {:name :uri :dir?} maps to display. The actual
;; directory listing is performed by the side-effecting layer (dapr.ui.events),
;; which sets :loading? while it runs and then calls browser-set-entries.

(defn browser-open
  "Open the folder browser at the top-level places list (entries loaded async)."
  [state]
  (assoc state :browser {:cwd nil :crumbs [] :entries [] :loading? true}))

(defn browser-close [state] (assoc state :browser nil))

(defn browser-set-entries
  "Record freshly listed `entries` and clear the loading flag."
  [state entries]
  (-> state
      (assoc-in [:browser :entries] (vec entries))
      (assoc-in [:browser :loading?] false)))

(defn browser-enter
  "Descend into child folder `{:keys [name label uri]}`: push a breadcrumb, make
  it the current directory, and mark loading."
  [state {:keys [uri] :as child}]
  (-> state
      (update-in [:browser :crumbs] (fnil conj []) {:label (or (:label child) (:name child)) :uri uri})
      (assoc-in [:browser :cwd] uri)
      (assoc-in [:browser :entries] [])
      (assoc-in [:browser :loading?] true)))

(defn browser-to-places
  "Return to the top-level places list."
  [state]
  (-> state
      (assoc-in [:browser :cwd] nil)
      (assoc-in [:browser :crumbs] [])
      (assoc-in [:browser :entries] [])
      (assoc-in [:browser :loading?] true)))

(defn browser-to-crumb
  "Jump back to the breadcrumb at index `idx`, dropping any deeper crumbs."
  [state idx]
  (let [crumbs (vec (take (inc idx) (get-in state [:browser :crumbs])))]
    (-> state
        (assoc-in [:browser :crumbs] crumbs)
        (assoc-in [:browser :cwd] (:uri (last crumbs)))
        (assoc-in [:browser :entries] [])
        (assoc-in [:browser :loading?] true))))

;; --- misc status -------------------------------------------------------------

(defn set-status [state status] (assoc state :status status))
(defn set-progress [state progress] (assoc state :progress progress))

(defn set-plan
  "Record a freshly computed plan and move to the :planned status."
  [state actions summary]
  (-> state
      (assoc :plan {:actions actions :summary summary})
      (assoc :status :planned)))

(defn append-log
  "Append a message line to the activity log, keeping only the most recent
  max-log-lines. :log-appends counts every append (never reset) so the view can
  detect new lines and auto-scroll even once the capped log stops growing."
  [state msg]
  (-> state
      (update :log (fn [log]
                     (let [log (conj log msg)
                           n   (count log)]
                       (if (> n max-log-lines)
                         (subvec log (- n max-log-lines))
                         log))))
      (update :log-appends inc)))

(defn set-error
  "Record an error message and move to the :error status."
  [state msg]
  (-> state
      (assoc :error msg)
      (assoc :status :error)))
