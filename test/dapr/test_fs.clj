(ns dapr.test-fs
  "Helpers for filesystem integration tests. Uses Google jimfs for low-level
  Path operations over a non-default provider, and real temp directories for the
  URI-driven catalog/sync path (file:// URIs are real and addressable). No mocks
  and no hardware — analogous to the Testcontainers approach used for backends."
  (:import (com.google.common.jimfs Configuration Jimfs)
           (java.nio.file FileSystem FileVisitOption Files LinkOption OpenOption Path)
           (java.nio.file.attribute FileAttribute)))

(defn unix-fs
  "Create a fresh in-memory unix FileSystem. Caller must close it."
  ^FileSystem []
  (Jimfs/newFileSystem (Configuration/unix)))

(defn sized-fs
  "In-memory unix FileSystem capped at `max-bytes` (its FileStore reports this
  via getTotalSpace/getUsableSpace). Caller must close it."
  ^FileSystem [max-bytes]
  (Jimfs/newFileSystem (-> (Configuration/unix)
                           (.toBuilder)
                           (.setMaxSize max-bytes)
                           (.build))))

(defn root
  "Create (and return) the directory at absolute `path` on `fs`."
  ^Path [^FileSystem fs ^String path]
  (let [p (.getPath fs path (make-array String 0))]
    (Files/createDirectories p (make-array FileAttribute 0))
    p))

(defn write!
  "Write `content` to Path `p`, creating parent directories. Returns `p`."
  ^Path [^Path p ^String content]
  (when-let [parent (.getParent p)]
    (Files/createDirectories parent (make-array FileAttribute 0)))
  (Files/write p (.getBytes content) (make-array OpenOption 0))
  p)

(defn slurp-path
  "Read Path `p` as a string."
  [^Path p]
  (String. (Files/readAllBytes p)))

(defn exists?
  [^Path p]
  (Files/exists p (make-array LinkOption 0)))

(defn temp-dir!
  "Create a real temporary directory. Returns its Path."
  ^Path []
  (Files/createTempDirectory "dapr-test" (make-array FileAttribute 0)))

(defn uri-of
  "file:// URI string of `p`."
  [^Path p]
  (str (.toUri p)))

(defn delete-tree!
  "Recursively delete `root` (children before parents)."
  [^Path root]
  (when (Files/exists root (make-array LinkOption 0))
    (with-open [^java.util.stream.Stream s (Files/walk root (make-array FileVisitOption 0))]
      (doseq [^Path p (sort-by #(- (count (str %))) (iterator-seq (.iterator s)))]
        (Files/deleteIfExists p)))))
