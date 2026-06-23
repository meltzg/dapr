(ns dapr.device.file.events
  (:require [dapr.device.events :as device-events]
            [dapr.device.file.fs :as file-fs]
            [dapr.device.fs :as device-fs]
            [dapr.state :as state]))

(defmethod device-events/open-browser! :file [_ state-atom]
  (swap! state-atom state/set-browser
         {:phase :browse :device/type :file :device nil :devices []
          :cwd nil :crumbs [] :entries [] :loading? true})
  (device-events/load-browser-entries! state-atom))

(defmethod device-events/browser-entries! :file [{:keys [cwd]}]
  (if cwd
    (device-fs/dir-children! cwd)
    (file-fs/local-places!)))
