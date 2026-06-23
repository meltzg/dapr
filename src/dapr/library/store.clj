(ns dapr.library.store
  "Side-effecting persistence of the user's libraries as EDN. Pure library
  helpers live in dapr.domain.library; this namespace only does file I/O
  (all fns end in !)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [dapr.fs.paths :as paths])
  (:import (java.io File PushbackReader)))

(defn default-path!
  "OS-appropriate path to libraries.edn under the user's config directory
  ($XDG_CONFIG_HOME, %APPDATA%, or ~/.config)."
  ^File []
  (let [base (or (System/getenv "XDG_CONFIG_HOME")
                 (System/getenv "APPDATA")
                 (io/file (paths/user-home) ".config"))]
    (io/file base "dapr" "libraries.edn")))

(defn load!
  "Read the vector of libraries from `path`; returns [] when the file is absent."
  [path]
  (let [f (io/file path)]
    (if (.exists f)
      (with-open [r (PushbackReader. (io/reader f))]
        (vec (edn/read r)))
      [])))

(defn save!
  "Write `libraries` (a vector) to `path` as pretty EDN, creating parent
  directories. Returns `libraries`."
  [path libraries]
  (let [f (io/file path)]
    (io/make-parents f)
    (spit f (with-out-str (pprint/pprint (vec libraries))))
    libraries))
