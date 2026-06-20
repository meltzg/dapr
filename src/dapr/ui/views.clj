(ns dapr.ui.views
  "Pure cljfx view descriptions for the Dapr window. Functions take the
  application state map and return cljfx data (no side effects). User events are
  dispatched to dapr.ui.events; formatting/predicates live in dapr.ui.format and
  dapr.domain.capacity."
  (:require [clojure.string :as str]
            [dapr.domain.capacity :as cap]
            [dapr.ui.events :as events]
            [dapr.ui.format :as fmt]))

;; --- library manager ---------------------------------------------------------

(defn- library-list [libraries]
  {:fx/type :v-box
   :spacing 4
   :children
   (into [{:fx/type :h-box :spacing 8 :alignment :center-left
           :children [{:fx/type :label :text "Libraries" :style "-fx-font-weight: bold;"}
                      {:fx/type :button :text "New…"
                       :on-action {:event/type ::events/library-new}}]}]
         (for [l libraries]
           {:fx/type :h-box :spacing 8 :alignment :center-left
            :children [{:fx/type :label :min-width 200
                        :text (format "%s  (%d dirs)" (:name l) (count (:roots l)))}
                       {:fx/type :button :text "Edit"
                        :on-action {:event/type ::events/library-edit :id (:id l)}}
                       {:fx/type :button :text "Delete"
                        :on-action {:event/type ::events/library-delete :id (:id l)}}]}))})

(defn- root-row [uri]
  {:fx/type :h-box :spacing 8 :alignment :center-left
   :children [{:fx/type :label :h-box/hgrow :always :text uri}
              {:fx/type :button :text "Remove"
               :on-action {:event/type ::events/editor-remove-root :uri uri}}]})

(defn- editor-panel [{:keys [name roots pending-uri]} mtp-candidates]
  {:fx/type :v-box
   :spacing 6
   :style "-fx-border-color: gray; -fx-border-radius: 4; -fx-padding: 8;"
   :children
   [{:fx/type :h-box :spacing 8 :alignment :center-left
     :children [{:fx/type :label :min-width 60 :text "Name"}
                {:fx/type :text-field :h-box/hgrow :always :text name
                 :on-text-changed {:event/type ::events/editor-name}}]}
    {:fx/type :label :text "Roots"}
    {:fx/type :v-box :spacing 2
     :children (if (seq roots)
                 (mapv root-row roots)
                 [{:fx/type :label :text "(no roots yet)"}])}
    {:fx/type :h-box :spacing 8 :alignment :center-left
     :children [{:fx/type :text-field :h-box/hgrow :always
                 :prompt-text "file:///… or mtp://…"
                 :text (or pending-uri "")
                 :on-text-changed {:event/type ::events/editor-pending-uri}}
                {:fx/type :button :text "Add URI"
                 :on-action {:event/type ::events/editor-add-pending}}
                {:fx/type :button :text "Add folder…"
                 :on-action {:event/type ::events/editor-browse}}]}
    {:fx/type :h-box :spacing 8 :alignment :center-left
     :children [{:fx/type :button :text "Detect MTP"
                 :on-action {:event/type ::events/editor-detect-mtp}}
                {:fx/type :combo-box :prompt-text "MTP storages"
                 :items (mapv :label mtp-candidates)
                 :on-value-changed {:event/type ::events/editor-add-candidate}}]}
    {:fx/type :h-box :spacing 8
     :children [{:fx/type :button :text "Save"
                 :on-action {:event/type ::events/editor-save}}
                {:fx/type :button :text "Cancel"
                 :on-action {:event/type ::events/editor-cancel}}]}]})

;; --- sync workflow -----------------------------------------------------------

(defn- library-combo [event-type value libraries]
  {:fx/type :combo-box
   :prompt-text "—"
   :items (mapv :name libraries)
   :value value
   :on-value-changed {:event/type event-type}})

(defn- sync-bar [libraries source-id sink-id]
  (let [name-of (fn [id] (:name (first (filter #(= (:id %) id) libraries))))]
    {:fx/type :h-box :spacing 8 :alignment :center-left
     :children [{:fx/type :label :text "Source"}
                (library-combo ::events/select-source (name-of source-id) libraries)
                {:fx/type :label :text "Sink"}
                (library-combo ::events/select-sink (name-of sink-id) libraries)]}))

(defn- capacity-bar [capacity]
  {:fx/type :h-box :spacing 8 :alignment :center-left
   :children [{:fx/type :label :min-width 70 :text "Capacity"}
              {:fx/type :progress-bar :h-box/hgrow :always :max-width Double/MAX_VALUE
               :progress (fmt/capacity-fraction capacity)}
              {:fx/type :label :text (fmt/capacity-text capacity)
               :style (if (fmt/over-capacity? capacity) "-fx-text-fill: red;" "")}]})

(defn- track-rows [{:keys [source-catalog sink-catalog selected free-bytes]}]
  (for [t (sort-by (juxt :name :rel) (vals source-catalog))
        :let [k    (:key t)
              on?  (contains? selected k)
              fits (cap/would-fit? k selected source-catalog sink-catalog free-bytes)]]
    {:fx/type  :check-box
     :text     (fmt/track-row-label t (get sink-catalog k))
     :selected on?
     :disable  (and (not on?) (not fits))
     :on-selected-changed {:event/type ::events/toggle-track :key k}}))

(defn- track-list [state]
  {:fx/type     :scroll-pane
   :v-box/vgrow :always
   :fit-to-width true
   :content     {:fx/type :v-box :spacing 2 :children (vec (track-rows state))}})

(defn- progress-bar [progress]
  {:fx/type   :progress-bar
   :max-width Double/MAX_VALUE
   :progress  (if (and progress (pos? (:total progress)))
                (/ (double (:done progress)) (:total progress))
                0.0)})

(defn- controls-row [state status]
  {:fx/type   :h-box
   :spacing   8
   :alignment :center-left
   :children  [{:fx/type :button :text "Preview"
                :disable (not (fmt/can-preview? state))
                :on-action {:event/type ::events/preview}}
               {:fx/type :button :text "Sync"
                :disable (not (fmt/can-sync? state))
                :on-action {:event/type ::events/sync}}
               {:fx/type :label :text (str "Status: " (fmt/status-text status))}]})

(defn root-view
  "Render the whole window from the application state map."
  [{:keys [libraries editor mtp-candidates source-id sink-id capacity plan
           progress status log] :as state}]
  {:fx/type :stage
   :showing true
   :title   "Dapr — music library sync"
   :width   860
   :height  680
   :on-close-request {:event/type ::events/quit}
   :scene
   {:fx/type :scene
    :root
    {:fx/type  :v-box
     :spacing  10
     :padding  12
     :children (cond-> [(library-list libraries)]
                 editor  (conj (editor-panel editor mtp-candidates))
                 :always (into [{:fx/type :separator}
                                (sync-bar libraries source-id sink-id)
                                (capacity-bar capacity)
                                (track-list state)
                                (controls-row state status)
                                {:fx/type :label :text (fmt/plan-summary-text (:summary plan))}
                                (progress-bar progress)
                                {:fx/type :text-area :pref-height 120 :editable false
                                 :text (str/join "\n" log)}]))}}})
