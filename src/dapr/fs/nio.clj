(ns dapr.fs.nio
  "Side-effecting filesystem adapter built purely on java.nio.file, so it works
  unchanged across providers — the default file:// provider and the mtp://
  provider supplied by melt-jfs. Pure data shaping lives in dapr.domain.*;
  everything here performs I/O (all fns end in !)."
  (:require [clojure.string :as str]
            [dapr.domain.library :as lib])
  (:import (java.net URI)
           (java.nio.file CopyOption DirectoryStream FileStore
                          FileSystemNotFoundException FileSystems FileVisitOption
                          Files LinkOption Path Paths StandardCopyOption)
           (java.nio.file.attribute BasicFileAttributes FileAttribute)))

(defn- ensure-filesystem!
  "Ensure the (possibly non-default) filesystem addressed by `uri` is open,
  opening it on demand. Returns the FileSystem."
  [^URI uri]
  (try
    (FileSystems/getFileSystem uri)
    (catch FileSystemNotFoundException _
      (FileSystems/newFileSystem uri ^java.util.Map {}))))

(defn root-path!
  "Resolve a root URI string to the java.nio.file.Path of its directory, opening
  a non-default filesystem (e.g. mtp://) on demand."
  ^Path [uri-str]
  (let [uri (URI. ^String uri-str)]
    (when-not (= "file" (str/lower-case (str (.getScheme uri))))
      (ensure-filesystem! uri))
    (Paths/get uri)))

(defn- relative-key
  "Relative path of `p` under `root`, as a string with '/' separators (so paths
  are comparable across filesystems with different separators)."
  [^Path root ^Path p]
  (-> (.relativize root p)
      (str)
      (str/replace "\\" "/")))

(defn- resolve-rel
  "Resolve a '/'-separated relative path string against `root` segment by
  segment, so it is valid on `root`'s filesystem regardless of its separator."
  ^Path [^Path root rel-path]
  (reduce (fn [^Path acc seg] (.resolve acc ^String seg))
          root
          (remove str/blank? (str/split rel-path #"/"))))

(defn- regular-files
  "Realized seq of every regular file under `root`."
  [^Path root]
  (with-open [^java.util.stream.Stream stream (Files/walk root (make-array FileVisitOption 0))]
    (->> (iterator-seq (.iterator stream))
         (filter (fn [^Path p] (Files/isRegularFile p (make-array LinkOption 0))))
         (doall))))

(defn- path->track
  "Build a track map for file `p` under root `uri` (Path `root`)."
  [^Path root uri ^Path p]
  (let [^BasicFileAttributes attrs
        (Files/readAttributes p BasicFileAttributes (make-array LinkOption 0))
        m {:name  (str (.getFileName p))
           :size  (.size attrs)
           :mtime (.toMillis (.lastModifiedTime attrs))
           :root  uri
           :rel   (relative-key root p)}]
    (assoc m :key (lib/track-key m))))

(defn catalog!
  "Scan every `root` URI of a library and return a seq of track maps for each
  audio file, tagged with the :root it lives under and its :rel path."
  ([roots] (catalog! roots lib/default-audio-extensions))
  ([roots extensions]
   (mapcat
    (fn [uri]
      (let [root (root-path! uri)]
        (->> (regular-files root)
             (filter (fn [^Path p] (lib/audio-file? (str (.getFileName p)) extensions)))
             (map (fn [p] (path->track root uri p))))))
    roots)))

(defn copy-file!
  "Copy file `rel-path` from `src-root` to `dst-root`, creating parent
  directories and replacing any existing file. Attributes are intentionally not
  copied: some providers (MTP) cannot set mtimes."
  [^Path src-root ^Path dst-root rel-path]
  (let [src    (resolve-rel src-root rel-path)
        dst    (resolve-rel dst-root rel-path)
        parent (.getParent dst)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/copy src dst
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))

(defn delete-file!
  "Delete file `rel-path` under `dst-root` if it exists."
  [^Path dst-root rel-path]
  (Files/deleteIfExists (resolve-rel dst-root rel-path)))

(defn root-free!
  "Placement input for one root: {:uri :free-bytes} (usable space of its
  backing device)."
  [uri]
  {:uri uri :free-bytes (.getUsableSpace (Files/getFileStore (root-path! uri)))})

(defn library-free!
  "Total usable bytes across the distinct devices backing `roots`, so two roots
  on the same device (e.g. two folders on a phone's SD card, or two folders on
  the same local disk) are not double counted. Stores are keyed by [name type],
  which distinguishes a phone's internal vs SD storage while still collapsing
  two folders on one device (JDK local FileStores do not override equals)."
  [roots]
  (->> roots
       (map (fn [uri] (Files/getFileStore (root-path! uri))))
       (reduce (fn [acc ^FileStore fs]
                 (assoc acc [(.name fs) (.type fs)] (.getUsableSpace fs)))
               {})
       (vals)
       (reduce + 0)))

(defn children!
  "Immediate child names directly under the root of `uri` (used to enumerate the
  storages of an MTP device)."
  [uri]
  (let [root (root-path! uri)]
    (with-open [^DirectoryStream stream (Files/newDirectoryStream root)]
      (mapv (fn [^Path p] (str (.getFileName p)))
            (iterator-seq (.iterator stream))))))
