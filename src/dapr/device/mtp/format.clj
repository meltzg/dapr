(ns dapr.device.mtp.format
  (:require [dapr.device.format :as device]))

(defmethod device/supported? :mtp [_] true)

(defmethod device/root-device-key :mtp [uri]
  (try
    (str "mtp://" (.getAuthority (java.net.URI. ^String uri)))
    (catch java.net.URISyntaxException _ nil)))

(defmethod device/selectable-root? :mtp [uri]
  (boolean (device/scheme uri)))

(defmethod device/library-menu-label :mtp [_]
  "📱  MTP device (mtp://)")

(defmethod device/browser-root-label :mtp [{:keys [device]}]
  (str "📱 " (:name device)))

(defmethod device/browser-current-location :mtp [{:keys [cwd]}]
  (or cwd "Pick a location to start"))
