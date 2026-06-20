(ns dapr.fs.mtp
  "MTP device enumeration via melt-jfs. This is the ONLY namespace that touches
  melt-jfs classes; it is loaded lazily (see dapr.ui.events/detect-devices!)
  so the default build, tests, and lint never require the melt-jfs jar or native
  libraries.

  Requires the :mtp alias (a built melt-jfs jar on the classpath, produced by
  `./gradlew jar` in the melt-jfs checkout) and the JVM option
  --enable-native-access=ALL-UNNAMED. File copy/delete/scan against an mtp:// URI
  go through the generic NIO code in dapr.fs.nio; only device discovery needs
  the melt-jfs API directly."
  (:import (org.meltzg.fs.mtp MTPDeviceBridge)))

(defn devices!
  "Detect connected MTP devices and return a vector of endpoints
  {:id <vendor:product:serial> :name <friendly-name> :uri \"mtp://<id>/\"}."
  []
  (let [bridge (MTPDeviceBridge/getInstance)]
    (->> (.getDeviceInfo bridge)
         (mapv (fn [entry]
                 (let [id-str (.toString ^Object (key entry))
                       info   (val entry)]
                   {:id   id-str
                    :name (.friendlyName info)
                    :uri  (str "mtp://" id-str "/")}))))))
