(ns dapr.domain.capacity-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.domain.capacity :as cap]))

(def source {[:a 10] {:size 10}
             [:b 20] {:size 20}
             [:c 30] {:size 30}
             [:x 50] {:size 50}})

(def sink {[:a 10] {:size 10}
           [:d 40] {:size 40}})

(deftest budget-test
  (testing "free space plus the bytes already occupied by sink tracks"
    (is (= 75 (cap/budget 25 sink))))
  (testing "nil free is treated as zero"
    (is (= 50 (cap/budget nil sink)))))

(deftest used-test
  (testing "sums sizes of the selected tracks (from either catalog)"
    (is (= 30 (cap/used #{[:a 10] [:b 20]} source sink)))
    (is (= 40 (cap/used #{[:d 40]} source sink))))
  (testing "empty selection is zero"
    (is (= 0 (cap/used #{} source sink)))))

(deftest usage-test
  (testing "snapshot of used / budget / free"
    (is (= {:used 30 :budget 75 :free 45}
           (cap/usage #{[:a 10] [:b 20]} source sink 25)))))

(deftest would-fit?-test
  (let [selected #{[:a 10] [:b 20]}]                ; used = 30, budget(25) = 75
    (testing "a new track that stays within budget fits"
      (is (true? (cap/would-fit? [:c 30] selected source sink 25))))
    (testing "a new track that exceeds the budget does not fit"
      (is (false? (cap/would-fit? [:x 50] selected source sink 25))))
    (testing "an already-selected key always fits"
      (is (true? (cap/would-fit? [:a 10] selected source sink 0))))
    (testing "a key already on the sink always fits"
      (is (true? (cap/would-fit? [:d 40] selected source sink 0))))))
