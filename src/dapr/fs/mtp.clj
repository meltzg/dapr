(ns dapr.fs.mtp
  "MTP device enumeration via melt-jfs. This is the ONLY namespace that touches
  melt-jfs classes; it is loaded lazily (see dapr.ui.events/browse-places!) so
  tests and lint never have to reach the native libraries.

  Device discovery needs native libmtp/WPD access at runtime, enabled by the JVM
  option --enable-native-access=ALL-UNNAMED (set in the :run and :dev aliases).
  File copy/delete/scan against an mtp:// URI go through the generic NIO code in
  dapr.fs.nio; only device discovery needs the melt-jfs API directly."
  (:require [clojure.string :as str])
  (:import (org.meltzg.fs.mtp MTPDeviceBridge)))

(defn- device-label
  "A non-blank display name for a device. libmtp often returns an empty
  friendlyName (the user never named the device), so fall back to the
  description, then the manufacturer, then the raw id."
  [info id-str]
  (or (->> [(.friendlyName info) (.description info) (.manufacturer info)]
           (remove str/blank?)
           (first))
      id-str))

(defn devices!
  "Detect connected MTP devices and return a vector of endpoints
  {:id <vendor:product:serial> :name <display-name> :uri \"mtp://<id>/\"}."
  []
  (let [bridge (MTPDeviceBridge/getInstance)]
    (->> (.getDeviceInfo bridge)
         (mapv (fn [entry]
                 (let [id-str (.toString ^Object (key entry))
                       info   (val entry)]
                   {:id   id-str
                    :name (device-label info id-str)
                    :uri  (str "mtp://" id-str "/")}))))))
