(ns dapr.device.file.tag-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.device.file.tag]
            [dapr.device.tag :as tag])
  (:import (java.nio.file Files OpenOption Path)
           (java.nio.file.attribute FileAttribute)))

(deftest default-dispatch-test
  (testing "smb:// and mtp:// roots fall back to path-derived tags"
    (is (= {:artist "Artist" :album "Album" :title "Title"}
           (tag/tags! {:root "smb://host/share/Artist/Album/Title.mp3"
                       :rel  "Artist/Album/Title.mp3"}
                      nil)))
    (is (= {:artist "Artist" :album "Album" :title "Title"}
           (tag/tags! {:root "mtp://dev/Artist/Album/Title.mp3"
                       :rel  "Artist/Album/Title.mp3"}
                      nil)))))

(deftest file-fallback-test
  (testing "an unreadable file:// audio file falls back to path-derived tags"
    (let [^Path p (Files/createTempFile "dapr-tag" ".mp3" (make-array FileAttribute 0))]
      (try
        (Files/write p (.getBytes "not really an mp3") (make-array OpenOption 0))
        (is (= {:artist "Artist" :album "Album" :title "Title"}
               (tag/tags! {:root (str (.toUri p))
                           :rel  "Artist/Album/Title.mp3"}
                          p)))
        (finally
          (Files/deleteIfExists p))))))

(deftest file-error-falls-back-test
  (testing "an Error from the tag reader (e.g. jaudiotagger StackOverflowError) is
            caught so one bad file can't abort the scan"
    (with-redefs [dapr.device.file.tag/read-tag (fn [_] (throw (StackOverflowError.)))]
      (is (= {:artist "Artist" :album "Album" :title "Title"}
             (tag/tags! {:root "file:///x/Artist/Album/Title.mp3"
                         :rel  "Artist/Album/Title.mp3"}
                        nil))))))
