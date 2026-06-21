(ns dapr.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.state :as state]))

(def lib-a {:id "a" :name "A" :roots ["file:///a"]})
(def lib-b {:id "b" :name "B" :roots ["file:///b"]})

(deftest libraries-test
  (testing "set, upsert (insert then replace), and lookup"
    (let [s (state/set-libraries state/initial-state [lib-a])]
      (is (= [lib-a] (:libraries s)))
      (is (= lib-a (state/library-by-id s "a")))
      (let [s2 (state/upsert-library s lib-b)]
        (is (= [lib-a lib-b] (:libraries s2)))
        (let [s3 (state/upsert-library s2 (assoc lib-a :name "A2"))]
          (is (= "A2" (:name (state/library-by-id s3 "a"))))
          (is (= 2 (count (:libraries s3)))))))))

(deftest delete-library-test
  (testing "removes the library and clears it from source/sink when selected"
    (let [s (-> state/initial-state
                (state/set-libraries [lib-a lib-b])
                (state/select-source "a")
                (state/select-sink "b")
                (state/delete-library "a"))]
      (is (= [lib-b] (:libraries s)))
      (is (nil? (:source-id s)))
      (is (= "b" (:sink-id s))))))

(deftest set-catalogs-test
  (testing "pre-selects sink tracks and computes capacity"
    (let [source {["a" 10] {:size 10 :key ["a" 10]}
                  ["b" 20] {:size 20 :key ["b" 20]}}
          sink   {["a" 10] {:size 10 :key ["a" 10]}}
          s (state/set-catalogs state/initial-state source sink 100)]
      (is (= #{["a" 10]} (:selected s)))
      ;; budget = 100 free + 10 on-sink = 110; used = selected (a) = 10
      (is (= {:used 10 :budget 110 :free 100} (:capacity s))))))

(deftest toggle-track-test
  (let [source {["a" 10] {:size 10 :key ["a" 10]}
                ["big" 100] {:size 100 :key ["big" 100]}}
        base (state/set-catalogs state/initial-state source {} 50)] ; budget 50
    (testing "selecting a fitting track adds it and updates capacity"
      (let [s (state/toggle-track base ["a" 10])]
        (is (= #{["a" 10]} (:selected s)))
        (is (= 10 (get-in s [:capacity :used])))))
    (testing "selecting an over-budget track is refused"
      (let [s (state/toggle-track base ["big" 100])]
        (is (= #{} (:selected s)))))
    (testing "deselecting always works"
      (let [s (-> base (state/toggle-track ["a" 10]) (state/toggle-track ["a" 10]))]
        (is (= #{} (:selected s)))))))

(deftest editor-test
  (testing "build, edit fields, add/remove roots, pending uri"
    (let [s (-> state/initial-state
                (state/set-editor {:id "x" :name "" :roots [] :pending-uri ""})
                (state/editor-name "Phone")
                (state/editor-add-root "file:///music")
                (state/editor-pending-uri "mtp://1:2:a/SD")
                (state/editor-add-pending))]
      (is (= "Phone" (get-in s [:editor :name])))
      (is (= ["file:///music" "mtp://1:2:a/SD"] (get-in s [:editor :roots])))
      (is (= "" (get-in s [:editor :pending-uri])))
      (testing "duplicate roots are ignored"
        (is (= ["file:///music" "mtp://1:2:a/SD"]
               (get-in (state/editor-add-root s "file:///music") [:editor :roots]))))
      (testing "remove and cancel"
        (is (= ["mtp://1:2:a/SD"]
               (get-in (state/editor-remove-root s "file:///music") [:editor :roots])))
        (is (nil? (:editor (state/cancel-editor s))))))))

(deftest browser-test
  (testing "open starts at places (nil cwd, loading), set-entries clears loading"
    (let [s (state/browser-open state/initial-state)]
      (is (= {:cwd nil :crumbs [] :entries [] :loading? true} (:browser s)))
      (let [s (state/browser-set-entries s [{:name "Music" :uri "file:///m" :dir? true}])]
        (is (false? (get-in s [:browser :loading?])))
        (is (= 1 (count (get-in s [:browser :entries])))))))
  (testing "entering folders pushes crumbs and tracks cwd"
    (let [s (-> state/initial-state
                (state/browser-open)
                (state/browser-enter {:name "Phone / SD" :uri "mtp://1:2:a/SD"})
                (state/browser-enter {:name "Music" :uri "mtp://1:2:a/SD/Music"}))]
      (is (= "mtp://1:2:a/SD/Music" (get-in s [:browser :cwd])))
      (is (= [{:label "Phone / SD" :uri "mtp://1:2:a/SD"}
              {:label "Music" :uri "mtp://1:2:a/SD/Music"}]
             (get-in s [:browser :crumbs])))
      (is (true? (get-in s [:browser :loading?])))
      (testing "jumping to a crumb truncates deeper crumbs and resets cwd"
        (let [s (state/browser-to-crumb s 0)]
          (is (= "mtp://1:2:a/SD" (get-in s [:browser :cwd])))
          (is (= [{:label "Phone / SD" :uri "mtp://1:2:a/SD"}]
                 (get-in s [:browser :crumbs])))))
      (testing "returning to places clears cwd and crumbs"
        (let [s (state/browser-to-places s)]
          (is (nil? (get-in s [:browser :cwd])))
          (is (= [] (get-in s [:browser :crumbs])))))
      (testing "close removes the browser entirely"
        (is (nil? (:browser (state/browser-close s))))))))

(deftest set-plan-test
  (testing "records the plan and moves to :planned"
    (let [s (state/set-plan state/initial-state [:action] {:add 1})]
      (is (= {:actions [:action] :summary {:add 1}} (:plan s)))
      (is (= :planned (:status s))))))

(deftest append-log-test
  (testing "appends messages in order"
    (is (= ["a" "b"] (:log (-> state/initial-state
                               (state/append-log "a")
                               (state/append-log "b"))))))
  (testing "caps retained lines at max-log-lines, keeping the most recent"
    (let [n (+ state/max-log-lines 50)
          s (reduce (fn [s i] (state/append-log s (str i)))
                    state/initial-state
                    (range n))]
      (is (= state/max-log-lines (count (:log s))))
      (is (= (str (dec n)) (last (:log s))))
      (is (= (str (- n state/max-log-lines)) (first (:log s))))
      (testing ":log-appends counts every append, not just retained lines"
        (is (= n (:log-appends s)))))))

(deftest set-error-test
  (testing "records the message and moves to :error"
    (let [s (state/set-error state/initial-state "boom")]
      (is (= "boom" (:error s)))
      (is (= :error (:status s))))))
