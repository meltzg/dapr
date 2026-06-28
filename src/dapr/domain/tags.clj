(ns dapr.domain.tags
  "Pure derivation of audio metadata (artist/album/title) from a track's path,
  used when no real tag reader applies (the default for every device) and as the
  per-field fallback when a file's embedded tag is missing a value. Nothing here
  performs I/O — given a track map it returns {:artist :album :title}, with nil
  for anything the path can't supply."
  (:require [clojure.string :as str]))

(defn- strip-extension
  "Filename `s` without its trailing extension (the last dot and after)."
  [s]
  (let [i (str/last-index-of s ".")]
    (if (and i (pos? i)) (subs s 0 i) s)))

(defn- segments
  "Non-blank '/'-separated segments of a track's :rel path (falling back to its
  :name when there is no :rel), so the same logic works across filesystems."
  [{:keys [rel name]}]
  (->> (str/split (or (not-empty rel) name "") #"/")
       (remove str/blank?)
       (vec)))

(defn from-path
  "Best-effort {:artist :album :title} derived from a track's path. The title is
  the filename without its extension; the outermost folder is always the artist;
  and when the track is nested at least two folders deep, its immediate parent
  folder is the album. So foo/bar/title.flac is artist foo / album bar, while a
  single folder bar/title.flac is artist bar with no album. Segments the path
  can't supply are nil."
  [track]
  (let [segs    (segments track)
        n       (count segs)
        folders (subvec segs 0 (max 0 (dec n)))
        fc      (count folders)]
    {:artist (when (pos? fc) (first folders))
     :album  (when (>= fc 2) (peek folders))
     :title  (when (pos? n) (strip-extension (peek segs)))}))
