(ns dapr.device.file.tag
  "Local file:// audio-tag reader, backed by jaudiotagger. Reads embedded ID3 /
  Vorbis / MP4 tags from the real file and falls back per field to the
  path-derived value when a tag is absent, the file carries no tag at all, or the
  read fails — so a row always has at least the path's best guess, and no single
  unreadable file can abort a whole library scan."
  (:require [clojure.string :as str]
            [dapr.device.tag :as tag]
            [dapr.domain.tags :as tags])
  (:import (java.nio.file Path)
           (java.util.logging Level Logger)
           (org.jaudiotagger.audio AudioFileIO)
           (org.jaudiotagger.tag FieldKey)))

;; jaudiotagger logs verbosely through java.util.logging on every read; silence it
;; so scans don't flood stderr.
(.setLevel (Logger/getLogger "org.jaudiotagger") Level/OFF)

(defn- read-tag
  "The embedded Tag of the audio file at `path`, or nil when it has none. May
  throw — including Errors from jaudiotagger — on a malformed file."
  [^Path path]
  (.getTag (AudioFileIO/read (.toFile path))))

(defmethod tag/tags! :file [track ^Path path]
  (let [fallback (assoc (tags/from-path track) :source :path)]
    (try
      (if-let [tg (read-tag path)]
        ;; The file has a real tag: :embedded, even where individual fields fall
        ;; back to the path (a blank embedded field).
        (let [pick (fn [^FieldKey k fb]
                     (let [v (.getFirst tg k)] (if (str/blank? v) fb v)))]
          {:artist (pick FieldKey/ARTIST (:artist fallback))
           :album  (pick FieldKey/ALBUM (:album fallback))
           :title  (pick FieldKey/TITLE (:title fallback))
           :source :embedded})
        fallback)
      ;; Throwable, not Exception: jaudiotagger can throw Errors (e.g. a
      ;; StackOverflowError on a malformed/deeply-nested tag) which would
      ;; otherwise escape and abort the entire scan.
      (catch Throwable _ fallback))))
