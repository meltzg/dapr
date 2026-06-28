(ns dapr.fs.nio-test
  "Unit coverage for the directory walk, using jimfs for real Path I/O over a
  non-default provider. The walk's URI-driven entry point (catalog!) needs a
  registered provider, so it lives in the integration suite; here we drive the
  private walker with a jimfs root directly."
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.domain.library :as lib]
            [dapr.fs.nio :as nio]
            [dapr.test-fs :as tfs]))

(def ^:private walk! #'nio/walk-audio-tracks!)

(deftest deep-nesting-no-overflow-test
  (with-open [fs (tfs/unix-fs)]
    (let [root (tfs/root fs "/m")
          ;; Far deeper than any call-stack recursion could handle — the iterative
          ;; walk must traverse it without a StackOverflowError.
          deep (reduce (fn [^java.nio.file.Path p _] (.resolve p "d")) root (range 20000))]
      (tfs/write! (.resolve root "top.mp3") "x")
      (tfs/write! (.resolve deep "bottom.mp3") "y")
      (let [tracks (walk! root "file:///m/" lib/default-audio-extensions nil)
            names  (set (map :name tracks))]
        (testing "every file is found regardless of nesting depth (no stack overflow)"
          (is (contains? names "top.mp3"))
          (is (contains? names "bottom.mp3")))))))
