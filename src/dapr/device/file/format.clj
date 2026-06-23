(ns dapr.device.file.format
  (:require [dapr.device.format :as device]))

(defmethod device/supported? :file [_] true)

(defmethod device/root-device-key :file [_]
  "file")

(defmethod device/selectable-root? :file [uri]
  (boolean (device/scheme uri)))

(defmethod device/library-menu-label :file [_]
  "💻  Local files (file://)")

(defmethod device/browser-root-label :file [_]
  "Places")

(defmethod device/browser-current-location :file [{:keys [cwd]}]
  (or cwd "Pick a location to start"))
