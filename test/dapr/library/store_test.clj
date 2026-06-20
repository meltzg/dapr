(ns dapr.library.store-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.library.store :as store]
            [dapr.test-fs :as tfs])
  (:import (java.nio.file Files)))

(deftest load!-missing-test
  (testing "loading an absent file yields an empty vector"
    (let [d (tfs/temp-dir!)]
      (try
        (is (= [] (store/load! (str d "/does-not-exist.edn"))))
        (finally
          (tfs/delete-tree! d))))))

(deftest save!-load!-round-trip-test
  (testing "saved libraries are read back identically, creating parent dirs"
    (let [d (tfs/temp-dir!)]
      (try
        (let [path (str d "/nested/libraries.edn")
              libs [{:id "1" :name "Phone" :roots ["file:///music" "mtp://1:2:a/SD"]}
                    {:id "2" :name "Laptop" :roots ["file:///home/me/Music"]}]]
          (store/save! path libs)
          (is (Files/exists (.resolve d "nested/libraries.edn") (make-array java.nio.file.LinkOption 0)))
          (is (= libs (store/load! path))))
        (finally
          (tfs/delete-tree! d))))))
