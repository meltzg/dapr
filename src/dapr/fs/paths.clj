(ns dapr.fs.paths
  "Shared OS path helpers. Isolated here so the user's home directory is read in
  exactly one place rather than duplicated across the library and device layers."
  (:require [clojure.java.io :as io])
  (:import (java.io File)))

(defn user-home
  "The current user's home directory as a java.io.File."
  ^File []
  (io/file (System/getProperty "user.home")))
