(ns dapr.ui.views
  "Pure cljfx view descriptions for the Dapr window. Functions take the
  application state map and return cljfx data (no side effects). User events are
  dispatched to dapr.ui.events; formatting/predicates live in dapr.ui.format and
  dapr.domain.capacity."
  (:require [cljfx.api :as fx]
            [clojure.string :as str]
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
                       :items [{:fx/type :menu-item :text "💻  Local files (file://)"
                                :on-action {:event/type ::events/library-new :kind :file}}
                               {:fx/type :menu-item :text "📱  MTP device (mtp://)"
                                :on-action {:event/type ::events/library-new :kind :mtp}}
                               {:fx/type :menu-item :text "🗄  SMB share (smb://)"
                                :on-action {:event/type ::events/library-new :kind :smb}}]}]}]
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

(defn- browser-crumbs
  "Breadcrumb trail: a root button (the device for mtp://, the share for smb://,
  else 'Places') followed by each descended folder."
  [kind device crumbs]
  {:fx/type :h-box :spacing 4 :alignment :center-left
   :children (into [{:fx/type :button
                     :text (case kind
                             :mtp (str "📱 " (:name device))
                             :smb (str "🗄 " (:name device))
                             "Places")
                     :on-action {:event/type ::events/browser-places}}]
                   (map-indexed
                    (fn [i c]
                      {:fx/type :button :text (str "▸ " (:label c))
                       :on-action {:event/type ::events/browser-crumb :idx i}})
                    crumbs))})

(defn- browser-entry-row [entry]
  {:fx/type :button
   :max-width Double/MAX_VALUE
   :alignment :baseline-left
   :text (str "📁  " (or (:label entry) (:name entry)))
   :on-action {:event/type ::events/browser-enter :child entry}})

(defn- browser-device-chooser
  "MTP only: choose which connected device to browse. A device that would mix with
  the library's existing MTP root (`allowed`) is disabled."
  [allowed {:keys [devices loading?]}]
  {:fx/type :v-box :spacing 6
   :children
   (into [{:fx/type :label :text "Select an MTP device"}]
         (cond
           loading?      [{:fx/type :label :text "Detecting devices…"}]
           (seq devices) (mapv (fn [d]
                                 {:fx/type :button :max-width Double/MAX_VALUE
                                  :alignment :baseline-left :text (str "📱  " (:name d))
                                  :disable (and (some? allowed)
                                                (not= allowed (lib/device-key (:uri d))))
                                  :on-action {:event/type ::events/browser-device :device d}})
                               devices)
           :else         [{:fx/type :label :text "(no MTP devices found)"}]))})

(defn- browser-connect-form
  "SMB only: enter the share URL and optional credentials, then connect. A blank
  username connects as guest; credentials entered here are saved to the OS
  keystore (never to the library file)."
  [{:keys [url username password workgroup loading?]}]
  {:fx/type :v-box :spacing 6
   :children
   [{:fx/type :label :text "Connect to an SMB server or share"}
    {:fx/type :text-field :prompt-text "smb://host/  (lists shares)  or  smb://host/share/" :text (or url "")
     :on-text-changed {:event/type ::events/browser-connect-field :field :url}}
    {:fx/type :h-box :spacing 6 :alignment :center-left
     :children [{:fx/type :text-field :h-box/hgrow :always
                 :prompt-text "Username (blank = guest)" :text (or username "")
                 :on-text-changed {:event/type ::events/browser-connect-field :field :username}}
                {:fx/type :text-field :h-box/hgrow :always
                 :prompt-text "Workgroup (optional)" :text (or workgroup "")
                 :on-text-changed {:event/type ::events/browser-connect-field :field :workgroup}}]}
    {:fx/type :password-field :prompt-text "Password" :text (or password "")
     :on-text-changed {:event/type ::events/browser-connect-field :field :password}}
    {:fx/type :label :wrap-text true :style "-fx-text-fill: gray;"
     :text "The password is stored in your OS keystore, not in the library file."}
    {:fx/type :button :text (if loading? "Connecting…" "Connect")
     :disable (boolean loading?)
     :on-action {:event/type ::events/browser-connect}}]})

(defn- browser-folders
  "Navigate directories within the chosen file:// scope or mtp:// device."
  [{:keys [kind device cwd crumbs entries loading?]}]
  {:fx/type :v-box :spacing 6
   :children
   [(browser-crumbs kind device crumbs)
    {:fx/type :scroll-pane :fit-to-width true :min-height 200 :pref-height 220
     :content {:fx/type :v-box :spacing 2
               :children (cond
                           loading?       [{:fx/type :label :text "Loading…"}]
                           (seq entries)  (mapv browser-entry-row entries)
                           :else          [{:fx/type :label :text "(no sub-folders here)"}])}}
    ;; Keep the selected path on its own line (wrapping) so a long URI can never
    ;; crush the button next to it.
    {:fx/type :label :wrap-text true
     :text (cond
             (nil? cwd)               "Pick a location to start"
             (lib/smb-host-root? cwd) "Pick a share to continue"
             :else                    cwd)}
    {:fx/type :button :text "Use this folder"
     ;; A bare SMB host (smb://host/) lists shares to browse, but is not itself a
     ;; usable root — a share must be entered first.
     :disable (or (nil? cwd) (lib/smb-host-root? cwd))
     :on-action {:event/type ::events/browser-select}}]})

(defn- browser-panel [allowed {:keys [phase] :as browser}]
  {:fx/type :v-box :spacing 6
   :style "-fx-border-color: gray; -fx-border-radius: 4; -fx-padding: 8;"
   :children
   [{:fx/type :label :text "Browse for a folder" :style "-fx-font-weight: bold;"}
    (case phase
      :device  (browser-device-chooser allowed browser)
      :connect (browser-connect-form browser)
      (browser-folders browser))
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
  "Estimated height of the open folder browser, which varies by phase: the MTP
  device chooser is just a label plus one row per device, the SMB connect form is
  a fixed set of input rows, while the folder view has a fixed-height listing area
  (so it is roughly constant)."
  [{:keys [phase devices loading?]}]
  (let [chrome 74]                                  ; padding + title + spacing + Cancel
    (+ chrome
       (case phase
         :device  (+ 18 (* 34 (max 1 (if loading? 1 (count devices)))))  ; "Select…" + device rows
         :connect 220                                                    ; label + url/creds/password/note/button
         330))))                                                         ; crumbs + listing + path + button

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
  "The primary window: menu bar plus the sync workflow."
  [{:keys [libraries source-id sink-id capacity plan progress status log] :as state}]
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
     :center
     {:fx/type  :v-box
      :spacing  10
      :padding  12
      :children [(sync-bar libraries source-id sink-id)
                 (capacity-bar capacity)
                 (track-list state)
                 (controls-row state status)
                 {:fx/type :label :text (fmt/plan-summary-text (:summary plan))}
                 (progress-bar progress)
                 ;; :scroll-top grows with every appended line (and is large
                 ;; enough to clamp to the bottom), so the log stays pinned to
                 ;; the newest line as it streams in.
                 {:fx/type :text-area :pref-height 120 :editable false
                  :scroll-top (* (:log-appends state) 1.0e7)
                  :text (str/join "\n" log)}]}}}})

(defn root-view
  "Render the whole application: the main window plus the (modal) settings
  window, whose visibility is driven by the state."
  [state]
  {:fx/type fx/ext-many
   :desc    [(main-stage state) (settings-stage state)]})
