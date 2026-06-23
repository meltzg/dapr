(ns dapr.fs.nio
  "Side-effecting filesystem adapter built purely on java.nio.file, so it works
  unchanged across providers — the default file:// provider and the mtp://
  provider supplied by melt-jfs. Pure data shaping lives in dapr.domain.*;
  everything here performs I/O (all fns end in !)."
  (:require [clojure.string :as str]
            [dapr.domain.library :as lib]
            [dapr.fs.smb :as smb])
  (:import (java.io File)
           (java.net URI)
           (java.nio.file CopyOption DirectoryStream FileStore
                          FileSystemNotFoundException FileSystems
                          Files LinkOption Path Paths StandardCopyOption)
           (java.nio.file.attribute BasicFileAttributes FileAttribute)))

(defn- ensure-filesystem!
  "Ensure the (possibly non-default) filesystem addressed by `uri` is open,
  opening it on demand. Returns the FileSystem."
  [^URI uri]
  (try
    (FileSystems/getFileSystem uri)
    (catch FileSystemNotFoundException _
      (FileSystems/newFileSystem uri {}))))

(defn root-path!
  "Resolve a root URI string to the java.nio.file.Path of its directory, opening
  a non-default filesystem (e.g. mtp://) on demand. smb:// is delegated to
  dapr.fs.smb, which authenticates from the OS keystore and resolves the Path on
  its own host-keyed FileSystem — Paths/get cannot be used there because smb-nio
  caches FileSystems under a credential-bearing authority that the credential-free
  root URI cannot reproduce."
  ^Path [uri-str]
  (let [uri    (URI. ^String uri-str)
        scheme (str/lower-case (str (.getScheme uri)))]
    (case scheme
      "file" (Paths/get uri)
      "smb"  (smb/root-path! uri-str)
      (do (ensure-filesystem! uri)
          (Paths/get uri)))))

(defn- relative-key
  "Relative path of `p` under `root`, as a string with '/' separators (so paths
  are comparable across filesystems with different separators)."
  [^Path root ^Path p]
  (-> (.relativize root p)
      (str)
      (str/replace "\\" "/")))

(defn- resolve-rel
  "Resolve a '/'-separated relative path string against `root` segment by segment,
  so it is valid on `root`'s filesystem regardless of its separator. Every segment
  but the last is marked as a folder (trailing '/'), because smb-nio refuses to
  resolve a child against a path it considers a file — without this, copy/delete of
  any nested path over SMB throws (file:// and mtp:// are unaffected, normalizing
  the trailing slash away)."
  ^Path [^Path root rel-path]
  (let [segs (vec (remove str/blank? (str/split rel-path #"/")))
        last-i (dec (count segs))]
    (reduce (fn [^Path acc [i ^String seg]]
              (.resolve acc (if (= i last-i) seg (str seg "/"))))
            root
            (map-indexed vector segs))))

(defn- audio-track
  "Build a track map for audio file `p` (Path) under root `uri` (Path `root`)
  from the already-read `attrs`."
  [^Path root uri ^Path p ^BasicFileAttributes attrs]
  (let [m {:name  (str (.getFileName p))
           :size  (.size attrs)
           :mtime (.toMillis (.lastModifiedTime attrs))
           :root  uri
           :rel   (relative-key root p)}]
    (assoc m :key (lib/track-key m))))

(defn- walk-audio-tracks!
  "Depth-first walk of `root`, collecting a track map for every audio file. Opens
  each directory's stream explicitly (rather than via Files/walkFileTree) so that
  `on-scan` is notified *before* the per-directory listing call -- which over MTP
  is a single blocking native round-trip -- making it possible to pinpoint a
  directory whose listing hangs (the last :dir event before the freeze names it).
  Reads one attribute set per entry, the same as Files/walkFileTree would.

  `on-scan`, when supplied, is called with:
    {:type :dir     :rel <dir rel path>} as each directory is *entered*, before its
                                         listing call (so the last :dir before a
                                         freeze names the directory whose listing
                                         hung over MTP);
    {:type :listing :count <n>}          once that directory's children are listed,
                                         so progress totals can grow as the walk
                                         recurses (n is its immediate child count);
    {:type :entry}                       for every child visited, advancing the
                                         done count toward the total;
    {:type :file    :track <track map>}  for each audio file found.
  Entries that fail to stat, and sub-directories that fail to open, are skipped
  (matching Files/walkFileTree's visitFileFailed=CONTINUE). If `on-scan` throws an
  ex-info carrying :dapr/abort, the whole walk unwinds (used to cancel a scan that
  a newer one has superseded)."
  [^Path root uri extensions on-scan]
  (letfn [(walk! [^Path dir]
            (when on-scan (on-scan {:type :dir :rel (relative-key root dir)}))
            (with-open [^DirectoryStream stream (Files/newDirectoryStream dir)]
              ;; Realize the listing (one provider round-trip, as the old lazy
              ;; doseq already incurred) so the child count is known up front and
              ;; can drive overall scan progress.
              (let [entries (vec (iterator-seq (.iterator stream)))]
                (when on-scan (on-scan {:type :listing :count (count entries)}))
                (reduce
                 (fn [tracks ^Path p]
                   (when on-scan (on-scan {:type :entry}))
                   (if-let [^BasicFileAttributes attrs
                            (try (Files/readAttributes p BasicFileAttributes (make-array LinkOption 0))
                                 (catch Exception _ nil))]
                     (cond
                       (.isDirectory attrs)
                       ;; Skip a sub-directory that fails to open, but let an
                       ;; on-scan abort (a superseded scan, carrying ex-data)
                       ;; propagate.
                       (into tracks
                             (try (walk! p)
                                  (catch Exception e
                                    (when (:dapr/abort (ex-data e)) (throw e))
                                    [])))

                       (and (.isRegularFile attrs)
                            (lib/audio-file? (str (.getFileName p)) extensions))
                       (let [track (audio-track root uri p attrs)]
                         (when on-scan (on-scan {:type :file :track track}))
                         (conj tracks track))

                       :else tracks)
                     tracks))
                 []
                 entries))))]
    (walk! root)))

(defn catalog!
  "Scan every `root` URI of a library and return a seq of track maps for each
  audio file, tagged with the :root it lives under and its :rel path. `on-scan`,
  when supplied, receives per-directory and per-file scan events (see
  walk-audio-tracks!) for progress reporting and diagnostics."
  ([roots] (catalog! roots nil))
  ([roots on-scan] (catalog! roots on-scan lib/default-audio-extensions))
  ([roots on-scan extensions]
   (mapcat (fn [uri] (walk-audio-tracks! (root-path! uri) uri extensions on-scan)) roots)))

(defn copy-file!
  "Copy file `rel-path` from `src-root` to `dst-root`, creating intermediate
  directories and replacing any existing file. Attributes are intentionally not
  copied: some providers (MTP) cannot set mtimes.

  Parent directories are created only for a *nested* rel-path. For a top-level
  file the parent is `dst-root` itself, which already exists — and over SMB a
  share/library root reports isDirectory=false, so createDirectories would wrongly
  try to mkdir it and throw."
  [^Path src-root ^Path dst-root rel-path]
  (let [src (resolve-rel src-root rel-path)
        dst (resolve-rel dst-root rel-path)]
    (when (str/includes? rel-path "/")
      (Files/createDirectories (.getParent dst) (make-array FileAttribute 0)))
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

(defn dir-children!
  "Immediate sub-directories directly under `uri`, each as
  {:name <file-name> :uri <child-uri-string> :dir? true}, sorted by name. Only
  directories are returned — the folder browser navigates directories, while the
  audio files inside them are discovered later by catalog!. Works unchanged
  across providers (file://, mtp://) since both build child URIs via Path/toUri.

  At an SMB server root (smb://host/) the entries are the host's shares, which
  smb-nio reports with isDirectory=false; those are surfaced as navigable folders
  so the browser can list shares, minus the hidden admin shares (IPC$, …)."
  [uri]
  (let [root        (root-path! uri)
        smb-shares? (lib/smb-host-root? uri)
        keep?       (fn [^Path p]
                      (if smb-shares?
                        (not (str/ends-with? (str (.getFileName p)) "$"))
                        (Files/isDirectory p (make-array LinkOption 0))))]
    (with-open [^DirectoryStream stream (Files/newDirectoryStream root)]
      (->> (iterator-seq (.iterator stream))
           (filter keep?)
           (map (fn [^Path p]
                  ;; Directory URIs end in '/': file:// and mtp:// already do; smb-nio
                  ;; needs it so a re-entered child is treated as a folder, not a file.
                  (let [u (str (.toUri p))]
                    {:name  (str (.getFileName p))
                     :uri   (if (str/ends-with? u "/") u (str u "/"))
                     :dir?  true})))
           (sort-by :name)
           (vec)))))

(defn local-places!
  "Top-level local browsing locations: each filesystem root plus the user's home
  directory, as {:name :uri :dir? true} entries. These seed the folder browser
  alongside any connected MTP storages."
  []
  (let [home  (File. (System/getProperty "user.home"))
        entry (fn [^File f label] {:name label :uri (str (.toURI f)) :dir? true})]
    (-> (mapv (fn [^File r] (entry r (str "Computer " (.getPath r))))
              (File/listRoots))
        (conj (entry home (str "Home " (.getPath home)))))))
