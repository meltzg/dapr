(ns dapr.domain.library-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.domain.library :as lib]))

(defn- track [name size root rel]
  {:name name :size size :root root :rel rel})

(deftest scheme-test
  (testing "extracts and lowercases the scheme"
    (is (= "file" (lib/scheme "file:///music")))
    (is (= "mtp" (lib/scheme "MTP://1:2:abc/Storage"))))
  (testing "nil for non-strings or schemeless input"
    (is (nil? (lib/scheme nil)))
    (is (nil? (lib/scheme 42)))
    (is (nil? (lib/scheme "/plain/path")))))

(deftest supported-scheme?-test
  (testing "file and mtp are supported"
    (is (true? (lib/supported-scheme? "file:///x")))
    (is (true? (lib/supported-scheme? "mtp://1:2:a/S"))))
  (testing "other schemes are not"
    (is (false? (lib/supported-scheme? "http://example.com")))
    (is (false? (lib/supported-scheme? "/plain/path")))))

(deftest library-valid?-test
  (testing "a named library with supported roots is valid"
    (is (true? (lib/library-valid? {:name "Phone" :roots ["file:///a" "mtp://1:2:a/S"]}))))
  (testing "invalid without a name, without roots, or with a bad root"
    (is (false? (lib/library-valid? {:name "" :roots ["file:///a"]})))
    (is (false? (lib/library-valid? {:name "  " :roots ["file:///a"]})))
    (is (false? (lib/library-valid? {:name "X" :roots []})))
    (is (false? (lib/library-valid? {:name "X" :roots ["http://x"]})))
    (is (false? (lib/library-valid? nil)))))

(deftest extension-test
  (testing "lowercased extension without the dot"
    (is (= "mp3" (lib/extension "song.MP3")))
    (is (= "flac" (lib/extension "a/b/c.flac"))))
  (testing "nil when none"
    (is (nil? (lib/extension "noext")))
    (is (nil? (lib/extension "trailingdot.")))
    (is (nil? (lib/extension nil)))))

(deftest audio-file?-test
  (testing "matches audio extensions, rejects others"
    (is (true? (lib/audio-file? "x.mp3")))
    (is (true? (lib/audio-file? "x.FLAC")))
    (is (false? (lib/audio-file? "cover.jpg")))
    (is (false? (lib/audio-file? "playlist.m3u"))))
  (testing "honors a custom extension set"
    (is (true? (lib/audio-file? "x.dsf" #{"dsf"})))
    (is (false? (lib/audio-file? "x.mp3" #{"dsf"})))))

(deftest track-key-test
  (testing "identity is [rel size], with the root excluded"
    (is (= ["a/song.mp3" 42] (lib/track-key {:name "song.mp3" :size 42 :rel "a/song.mp3"})))))

(deftest catalog-test
  (testing "indexes tracks by [rel size]; first wins when two roots share a key"
    (let [a  (track "a.mp3" 1 "r1" "a.mp3")
          b  (track "b.mp3" 2 "r1" "sub/b.mp3")
          a2 (track "a.mp3" 1 "r2" "a.mp3")    ; same rel+size under another root
          cat (lib/catalog [a b a2])]
      (is (= #{["a.mp3" 1] ["sub/b.mp3" 2]} (set (keys cat))))
      (is (= "r1" (:root (cat ["a.mp3" 1])))))))

(deftest track-total-size-test
  (testing "sums sizes"
    (is (= 6 (lib/track-total-size [{:size 1} {:size 2} {:size 3}]))))
  (testing "empty is zero"
    (is (= 0 (lib/track-total-size [])))))

(deftest initial-selection-test
  (testing "pre-selects every key on the sink"
    (is (= #{["a.mp3" 1] ["b.mp3" 2]}
           (lib/initial-selection {["a.mp3" 1] {} ["b.mp3" 2] {}}))))
  (testing "empty sink selects nothing"
    (is (= #{} (lib/initial-selection {})))))
