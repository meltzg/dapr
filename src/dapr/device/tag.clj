(ns dapr.device.tag
  "Audio-tag extension point by device type. Device-specific namespaces install a
  method that reads embedded tags from the underlying file; the default derives
  artist/album/title from the track's path (see dapr.domain.tags), so devices
  without a real tag reader (smb://, mtp://) still get usable metadata. Methods
  return {:artist :album :title :source} — where :source is :embedded when the
  values came from the file's own tags and :path when inferred from the path — and
  run during the scan (see dapr.fs.nio)."
  (:require [dapr.device.format :as device]
            [dapr.domain.tags :as tags]))

(defmulti tags!
  "Read {:artist :album :title :source} for `track`, given its already-resolved
  nio `path`. Dispatches on the track root's device type."
  (fn [track _path] (device/device-type (:root track))))

(defmethod tags! :default [track _path]
  (assoc (tags/from-path track) :source :path))
