(ns dapr.domain.library-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.domain.library :as lib]))

(defn- track [name size root rel]
  {:name name :size size :root root :rel rel})

(deftest scheme-test
  (testing "extracts and lowercases the scheme"
    (is (= "file" (lib/scheme "file:///music")))
    (is (= "mtp" (lib/scheme "MTP://1:2:abc/Storage")))
    (is (= "smb" (lib/scheme "SMB://nas/Music/"))))
  (testing "nil for non-strings or schemeless input"
    (is (nil? (lib/scheme nil)))
    (is (nil? (lib/scheme 42)))
    (is (nil? (lib/scheme "/plain/path")))))

(deftest supported-scheme?-test
  (testing "file, mtp and smb are supported"
    (is (true? (lib/supported-scheme? "file:///x")))
    (is (true? (lib/supported-scheme? "mtp://1:2:a/S")))
    (is (true? (lib/supported-scheme? "smb://nas/Music/"))))
  (testing "other schemes are not"
    (is (false? (lib/supported-scheme? "http://example.com")))
    (is (false? (lib/supported-scheme? "/plain/path")))))

(deftest library-valid?-test
  (testing "a named library whose roots share one device is valid"
    (is (true? (lib/library-valid? {:name "Music" :roots ["file:///a" "file:///b"]})))
    (is (true? (lib/library-valid? {:name "Phone" :roots ["mtp://1:2:a/S" "mtp://1:2:a/Music"]})))
    (is (true? (lib/library-valid? {:name "NAS" :roots ["smb://nas/Music/a/" "smb://nas/Music/b/"]}))))
  (testing "invalid without a name, without roots, or with a bad root"
    (is (false? (lib/library-valid? {:name "" :roots ["file:///a"]})))
    (is (false? (lib/library-valid? {:name "  " :roots ["file:///a"]})))
    (is (false? (lib/library-valid? {:name "X" :roots []})))
    (is (false? (lib/library-valid? {:name "X" :roots ["http://x"]})))
    (is (false? (lib/library-valid? nil))))
  (testing "invalid when roots mix devices"
    (is (false? (lib/library-valid? {:name "X" :roots ["file:///a" "mtp://1:2:a/S"]})))
    (is (false? (lib/library-valid? {:name "X" :roots ["mtp://1:2:a/S" "mtp://9:9:z/S"]})))
    (is (false? (lib/library-valid? {:name "X" :roots ["smb://nas/Music/" "smb://nas/Photos/"]}))))
  (testing "invalid when a root is a bare SMB host (no share chosen)"
    (is (false? (lib/library-valid? {:name "X" :roots ["smb://nas/"]})))
    (is (false? (lib/library-valid? {:name "X" :roots ["smb://nas"]})))))

(deftest smb-host-root?-test
  (testing "a bare SMB host is a share-less browse location"
    (is (true? (lib/smb-host-root? "smb://nas/")))
    (is (true? (lib/smb-host-root? "smb://nas"))))
  (testing "a share (with or without sub-path) is not"
    (is (false? (lib/smb-host-root? "smb://nas/Music/")))
    (is (false? (lib/smb-host-root? "smb://nas/Music/sub/"))))
  (testing "non-smb URIs are never host roots"
    (is (false? (lib/smb-host-root? "file:///a")))
    (is (false? (lib/smb-host-root? "mtp://1:2:a/S")))
    (is (false? (lib/smb-host-root? nil)))))

(deftest device-key-test
  (testing "all file:// roots share one key, each MTP device gets its own"
    (is (= "file" (lib/device-key "file:///music")))
    (is (= "file" (lib/device-key "file:///other/disk")))
    (is (= "mtp://1:2:a" (lib/device-key "mtp://1:2:a/SD/Music")))
    (is (not= (lib/device-key "mtp://1:2:a/S") (lib/device-key "mtp://9:9:z/S"))))
  (testing "each SMB share gets its own key, independent of sub-path"
    (is (= "smb://nas/Music" (lib/device-key "smb://nas/Music/")))
    (is (= "smb://nas/Music" (lib/device-key "smb://nas/Music/sub/dir/")))
    (is (not= (lib/device-key "smb://nas/Music/") (lib/device-key "smb://nas/Photos/")))
    (is (not= (lib/device-key "smb://nas/Music/") (lib/device-key "smb://other/Music/"))))
  (testing "nil for unsupported or unparseable URIs"
    (is (nil? (lib/device-key "http://x")))
    (is (nil? (lib/device-key nil)))))

(deftest root-addable?-test
  (testing "anything supported is addable to an empty library"
    (is (true? (lib/root-addable? [] "file:///a")))
    (is (true? (lib/root-addable? [] "mtp://1:2:a/S")))
    (is (false? (lib/root-addable? [] "http://x"))))
  (testing "only same-device roots may be added to a non-empty library"
    (is (true? (lib/root-addable? ["file:///a"] "file:///b")))
    (is (false? (lib/root-addable? ["file:///a"] "mtp://1:2:a/S")))
    (is (true? (lib/root-addable? ["mtp://1:2:a/S"] "mtp://1:2:a/Music")))
    (is (false? (lib/root-addable? ["mtp://1:2:a/S"] "mtp://9:9:z/S")))
    (is (false? (lib/root-addable? ["mtp://1:2:a/S"] "file:///a")))
    (is (true? (lib/root-addable? ["smb://nas/Music/"] "smb://nas/Music/sub/")))
    (is (false? (lib/root-addable? ["smb://nas/Music/"] "smb://nas/Photos/"))))
  (testing "a bare SMB host is never addable (a share must be chosen first)"
    (is (false? (lib/root-addable? [] "smb://nas/")))
    (is (false? (lib/root-addable? [] "smb://nas")))))

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
