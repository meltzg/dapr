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
   :editor         nil    ; library being added/edited, or nil
   :devices        []     ; detected MTP devices (root picking)
   :mtp-candidates []     ; [{:uri :label}] discovered MTP storages
   :status         :idle  ; :idle :scanning :planned :syncing :done :error
   :progress       nil    ; {:done n :total t}
   :log            []     ; vector of message strings
   :error          nil})

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

;; --- misc status -------------------------------------------------------------

(defn set-devices [state devices] (assoc state :devices devices))
(defn set-mtp-candidates [state candidates] (assoc state :mtp-candidates candidates))
(defn set-status [state status] (assoc state :status status))
(defn set-progress [state progress] (assoc state :progress progress))

(defn set-plan
  "Record a freshly computed plan and move to the :planned status."
  [state actions summary]
  (-> state
      (assoc :plan {:actions actions :summary summary})
      (assoc :status :planned)))

(defn append-log
  "Append a message line to the activity log."
  [state msg]
  (update state :log conj msg))

(defn set-error
  "Record an error message and move to the :error status."
  [state msg]
  (-> state
      (assoc :error msg)
      (assoc :status :error)))
