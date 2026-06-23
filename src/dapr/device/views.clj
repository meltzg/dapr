(ns dapr.device.views
  "Device-specific cljfx view extension points plus shared folder-browser widgets."
  (:require [dapr.device.format :as device-format]))

(defmulti library-menu-item
  "Menu item descriptor for creating a library of `device-type`."
  identity)

(defmulti browser-content
  "View content for the browser body."
  (fn [_allowed browser] [(:device/type browser) (:phase browser)]))

(defmulti browser-height
  "Estimated browser panel height for settings window sizing."
  (fn [browser] [(:device/type browser) (:phase browser)]))

(defmethod browser-content :default [_ _]
  {:fx/type :label :text "Unsupported browser state"})

(defmethod browser-height :default [_] 330)

(defn menu-item [device-type]
  {:fx/type :menu-item
   :text (device-format/library-menu-label device-type)
   :on-action {:event/type :dapr.ui.events/library-new :device/type device-type}})

(defn browser-crumbs
  "Generic breadcrumb trail: a device root button followed by descended folders."
  [browser]
  {:fx/type :h-box :spacing 4 :alignment :center-left
   :children (into [{:fx/type :button
                     :text (device-format/browser-root-label browser)
                     :on-action {:event/type :dapr.ui.events/browser-places}}]
                   (map-indexed
                    (fn [i c]
                      {:fx/type :button :text (str "▸ " (:label c))
                       :on-action {:event/type :dapr.ui.events/browser-crumb :idx i}})
                    (:crumbs browser)))})

(defn browser-entry-row [entry]
  {:fx/type :button
   :max-width Double/MAX_VALUE
   :alignment :baseline-left
   :text (str "📁  " (or (:label entry) (:name entry)))
   :on-action {:event/type :dapr.ui.events/browser-enter :child entry}})

(defn folder-browser
  "Generic directory browser once a device-specific setup phase has established
  the browse root."
  [{:keys [cwd entries loading?] :as browser}]
  {:fx/type :v-box :spacing 6
   :children
   [(browser-crumbs browser)
    {:fx/type :scroll-pane :fit-to-width true :min-height 200 :pref-height 220
     :content {:fx/type :v-box :spacing 2
               :children (cond
                           loading?      [{:fx/type :label :text "Loading…"}]
                           (seq entries) (mapv browser-entry-row entries)
                           :else         [{:fx/type :label :text "(no sub-folders here)"}])}}
    {:fx/type :label :wrap-text true
     :text (device-format/browser-current-location browser)}
    {:fx/type :button :text "Use this folder"
     :disable (or (nil? cwd) (not (device-format/selectable-root? cwd)))
     :on-action {:event/type :dapr.ui.events/browser-select}}]})
