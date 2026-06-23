(ns dapr.device.file.fs
  "Local file:// device support."
  (:require [dapr.device.fs :as dfs])
  (:import (java.io File)
           (java.net URI)
           (java.nio.file Paths)))

(defmethod dfs/root-path! :file [uri-str]
  (Paths/get (URI. ^String uri-str)))

(defmethod dfs/dir-children! :file [uri]
  (dfs/directory-children! (dfs/root-path! uri) dfs/directory?))

(defn local-places!
  "Top-level local browsing locations: each filesystem root plus the user's home
  directory, as {:name :uri :dir? true} entries."
  []
  (let [home  (File. (System/getProperty "user.home"))
        entry (fn [^File f label] {:name label :uri (str (.toURI f)) :dir? true})]
    (-> (mapv (fn [^File r] (entry r (str "Computer " (.getPath r))))
              (File/listRoots))
        (conj (entry home (str "Home " (.getPath home)))))))
