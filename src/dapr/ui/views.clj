(ns dapr.ui.views
  "Pure cljfx view descriptions for the Dapr window. Functions take the
  application state map and return cljfx data (no side effects). User events are
  dispatched to dapr.ui.events; formatting/predicates live in dapr.ui.format and
  dapr.domain.capacity."
  (:require [cljfx.api :as fx]
            [clojure.string :as str]
            [dapr.device.file.views]
            [dapr.device.format :as device-format]
            [dapr.device.mtp.views]
            [dapr.device.smb.views]
            [dapr.device.views :as device-views]
            [dapr.domain.capacity :as cap]
            [dapr.domain.library :as lib]
            [dapr.ui.events :as events]
            [dapr.ui.format :as fmt])
  (:import (javafx.stage Screen)))

;; --- library manager ---------------------------------------------------------

(defn- library-list [libraries]
  {:fx/type :v-box
   :spacing 4
   :children
   (into [{:fx/type :h-box :spacing 8 :alignment :center-left
           :children [{:fx/type :label :text "Libraries" :style "-fx-font-weight: bold;"}
                      {:fx/type :menu-button :text "New…"
                       :items (mapv device-views/library-menu-item device-format/types)}]}]
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

(defn- browser-panel [allowed browser]
  {:fx/type :v-box :spacing 6
   :style "-fx-border-color: gray; -fx-border-radius: 4; -fx-padding: 8;"
   :children
   [{:fx/type :label :text "Browse for a folder" :style "-fx-font-weight: bold;"}
    (device-views/browser-content allowed browser)
    {:fx/type :h-box :alignment :center-right
     :children [{:fx/type :button :text "Cancel"
                 :on-action {:event/type ::events/browser-cancel}}]}]})

(defn- editor-panel [{:keys [name roots]} browser]
  {:fx/type :v-box
   :spacing 6
   :style "-fx-border-color: gray; -fx-border-radius: 4; -fx-padding: 8;"
   :children
   (cond-> [{:fx/type :h-box :spacing 8 :alignment :center-left
             :children [{:fx/type :label :min-width 60 :text "Name"}
                        {:fx/type :text-field :h-box/hgrow :always :text name
                         :on-text-changed {:event/type ::events/editor-name}}]}
            {:fx/type :label :text "Roots"}
            {:fx/type :v-box :spacing 2
             :children (if (seq roots)
                         (mapv root-row roots)
                         [{:fx/type :label :text "(no roots yet)"}])}
            {:fx/type :h-box :spacing 8 :alignment :center-left
             :children [{:fx/type :button :text "Browse…"
                         :disable (some? browser)
                         :on-action {:event/type ::events/editor-browse}}]}]
     browser (conj (browser-panel (lib/roots-device-key roots) browser))
     :always (conj {:fx/type :h-box :spacing 8
                    :children [{:fx/type :button :text "Save"
                                :on-action {:event/type ::events/editor-save}}
                               {:fx/type :button :text "Cancel"
                                :on-action {:event/type ::events/editor-cancel}}]}))})

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

(defn- track-items
  "Resolve the source catalog into a sorted vector of row maps for the track
  list, one per track: {:key :label :on? :disable}. Capacity is checked in
  constant time per row against the selection's remaining free bytes (computed
  once from :capacity), so this stays O(n) even for libraries of many thousands
  of tracks — see dapr.domain.capacity/row-fits?."
  [{:keys [source-catalog sink-catalog selected capacity]}]
  (let [free (:free capacity)]
    (->> (vals source-catalog)
         (sort-by (juxt :name :rel))
         (mapv (fn [t]
                 (let [k   (:key t)
                       on? (contains? selected k)]
                   {:key     k
                    :label   (fmt/track-row-label t (get sink-catalog k))
                    :on?     on?
                    :disable (and (not on?)
                                  (not (cap/row-fits? k (:size t) selected sink-catalog free)))}))))))

(defn- track-list
  "The source-track picker as a virtualized ListView: JavaFX realizes only the
  cells currently scrolled into view (a few dozen), not one node per track, so a
  multi-thousand-track library scrolls and toggles smoothly. Each cell is a
  checkbox; toggling it dispatches ::toggle-track."
  [state]
  {:fx/type      :list-view
   :v-box/vgrow  :always
   :items        (track-items state)
   :cell-factory {:fx/cell-type :list-cell
                  :describe (fn [{:keys [key label on? disable]}]
                              {:graphic {:fx/type  :check-box
                                         :text     label
                                         :selected on?
                                         :disable  disable
                                         :on-selected-changed {:event/type ::events/toggle-track :key key}}})}})

(defn- progress-bar [progress]
  {:fx/type    :progress-bar
   :max-width  Double/MAX_VALUE
   :min-height 18
   :progress   (if (and progress (pos? (:total progress)))
                 (/ (double (:done progress)) (:total progress))
                 0.0)})

(defn- status-bar
  "An always-visible strip pinned to the window bottom holding the scan/sync
  progress bar, so it can never be clipped by the resizable split above it."
  [progress]
  {:fx/type   :h-box
   :padding   8
   :alignment :center-left
   :children  [(assoc (progress-bar progress) :h-box/hgrow :always)]})

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

(defn- sync-pane
  "Top section of the workspace: the source/sink pickers, capacity meter, the
  track picker (which grows to fill the section), the action buttons and the plan
  summary."
  [{:keys [libraries source-id sink-id capacity plan status] :as state}]
  {:fx/type    :v-box
   :spacing    10
   :padding    12
   ;; Keep a floor on the sync area so dragging the divider all the way down can't
   ;; collapse it entirely.
   :min-height 200
   :children   [(sync-bar libraries source-id sink-id)
                (capacity-bar capacity)
                (track-list state)
                (controls-row state status)
                {:fx/type :label :text (fmt/plan-summary-text (:summary plan))}]})

(defn- activity-pane
  "Bottom section of the workspace: the activity log, which grows to fill whatever
  height the section is given."
  [{:keys [log log-appends]}]
  {:fx/type    :v-box
   :padding    12
   :min-height 120
   :children   [;; :scroll-top grows with every appended line (and is large enough
                ;; to clamp to the bottom), so the log stays pinned to the newest
                ;; line as it streams in.
                {:fx/type     :text-area
                 :v-box/vgrow :always
                 :editable    false
                 :scroll-top  (* log-appends 1.0e7)
                 :text        (str/join "\n" log)}]})

(defn- workspace
  "The main window body: the sync UI above a resizable activity-log panel, divided
  by a draggable splitter — drag it down to grow the sync area, up to grow the
  log, like an IDE's terminal panel. (The progress bar is a separate always-on
  status strip below this, so it is never affected by the divider.)"
  [state]
  {:fx/type           :split-pane
   :orientation       :vertical
   :divider-positions [0.62]
   :items             [(sync-pane state) (activity-pane state)]})

;; --- window assembly ---------------------------------------------------------

(defn- menu-bar []
  {:fx/type :menu-bar
   :menus
   [{:fx/type :menu :text "File"
     :items [{:fx/type :menu-item :text "Quit"
              :on-action {:event/type ::events/quit}}]}
    {:fx/type :menu :text "Settings"
     :items [{:fx/type :menu-item :text "Manage Libraries…"
              :on-action {:event/type ::events/settings-open}}]}]})

(defn- browser-panel-height
  "Estimated height of the open folder browser. Device-specific chooser/connect
  phases provide their own estimates; folder browsing is a fixed-height list."
  [browser]
  (+ 74 (device-views/browser-height browser)))

(defn- settings-height
  "Preferred settings-window height for the current content, so the window grows
  and shrinks with what it shows. Built additively from the body's actual parts —
  the library list (one row per library), or the editor (a fixed header plus one
  row per root, plus the folder browser when open) — rather than a blanket guess,
  so the window hugs its content. Capped at the screen height (less a margin),
  beyond which the body scrolls instead of growing further."
  [editor browser libraries]
  (let [chrome 96                          ; window chrome + padding + Close row
        body   (if editor
                 (+ 147                                       ; name/labels/add/save rows
                    (* 28 (max 1 (count (:roots editor))))    ; one row per root
                    (if browser (browser-panel-height browser) 0))
                 (+ 40 (* 32 (count libraries))))]            ; library list
    (min (- (.getHeight (.getVisualBounds (Screen/getPrimary))) 60)
         (+ chrome body))))

(defn- settings-stage
  "Modal window holding the library creation/management UI. Stays in the scene
  graph at all times; its visibility tracks :settings-open? in the state. Its
  height tracks the content (see settings-height); the body sits in a scroll-pane
  so it scrolls only once the window hits its screen-bounded maximum."
  [{:keys [settings-open? libraries editor browser]}]
  {:fx/type  :stage
   :showing  (boolean settings-open?)
   :modality :application-modal
   :title    "Settings — Libraries"
   :width    640
   :height   (settings-height editor browser libraries)
   :on-close-request {:event/type ::events/settings-close}
   :scene
   {:fx/type :scene
    :root
    {:fx/type :v-box
     :spacing 10
     :padding 12
     ;; While creating/editing, show only the editor; otherwise the library list.
     :children [{:fx/type :scroll-pane
                 :v-box/vgrow :always
                 :fit-to-width true
                 :content (if editor
                            (editor-panel editor browser)
                            (library-list libraries))}
                {:fx/type :h-box :alignment :center-right
                 :children [{:fx/type :button :text "Close"
                             :on-action {:event/type ::events/settings-close}}]}]}}})

(defn- main-stage
  "The primary window: menu bar plus the sync workspace (a resizable split of the
  sync UI over the progress + activity log)."
  [state]
  {:fx/type :stage
   :showing true
   :title   "Dapr — music library sync"
   :width   860
   :height  680
   :on-close-request {:event/type ::events/quit}
   :scene
   {:fx/type :scene
    :root
    {:fx/type :border-pane
     :top     (menu-bar)
     :center  (workspace state)
     :bottom  (status-bar (:progress state))}}})

(defn root-view
  "Render the whole application: the main window plus the (modal) settings
  window, whose visibility is driven by the state."
  [state]
  {:fx/type fx/ext-many
   :desc    [(main-stage state) (settings-stage state)]})
