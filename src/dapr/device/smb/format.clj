(ns dapr.device.smb.format
  (:require [clojure.string :as str]
            [dapr.device.format :as device])
  (:import (java.net URI URISyntaxException)))

(defn share
  "The share segment of an smb:// URI, or nil for a bare server root."
  [uri-str]
  (try
    (->> (str/split (str (.getPath (URI. ^String uri-str))) #"/")
         (remove str/blank?)
         (first))
    (catch URISyntaxException _ nil)))

(defn host-root?
  "True when `uri-str` is an SMB server root with no share chosen yet."
  [uri-str]
  (and (= :smb (device/device-type uri-str))
       (nil? (share uri-str))))

(defmethod device/supported? :smb [_] true)

(defmethod device/root-device-key :smb [uri]
  (try
    (str "smb://" (.getAuthority (URI. ^String uri)) "/" (share uri))
    (catch URISyntaxException _ nil)))

(defmethod device/selectable-root? :smb [uri]
  (and (boolean (device/scheme uri))
       (not (host-root? uri))))

(defmethod device/library-menu-label :smb [_]
  "🗄  SMB share (smb://)")

(defmethod device/browser-root-label :smb [{:keys [device]}]
  (str "🗄 " (:name device)))

(defmethod device/browser-current-location :smb [{:keys [cwd]}]
  (cond
    (nil? cwd)        "Pick a location to start"
    (host-root? cwd)  "Pick a share to continue"
    :else            cwd))
