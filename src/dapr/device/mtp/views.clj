(ns dapr.device.mtp.views
  (:require [dapr.domain.library :as lib]
            [dapr.device.views :as device-views]))

(defmethod device-views/library-menu-item :mtp [device-type]
  (device-views/menu-item device-type))

(defn- device-chooser
  "Choose which connected MTP device to browse. A device that would mix with the
  library's existing MTP root (`allowed`) is disabled."
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
                                  :on-action {:event/type :dapr.ui.events/browser-device :device d}})
                               devices)
           :else         [{:fx/type :label :text "(no MTP devices found)"}]))})

(defmethod device-views/browser-content [:mtp :device] [allowed browser]
  (device-chooser allowed browser))

(defmethod device-views/browser-content [:mtp :browse] [_ browser]
  (device-views/folder-browser browser))

(defmethod device-views/browser-height [:mtp :device] [{:keys [devices loading?]}]
  (+ 74 18 (* 34 (max 1 (if loading? 1 (count devices))))))

(defmethod device-views/browser-height [:mtp :browse] [_]
  330)
