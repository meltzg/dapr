(ns dapr.domain.tags-test
  (:require [clojure.test :refer [deftest is testing]]
            [dapr.domain.tags :as tags]))

(deftest from-path-test
  (testing "Artist/Album/Title.ext layout maps each segment"
    (is (= {:artist "Artist" :album "Album" :title "Title"}
           (tags/from-path {:rel "Artist/Album/Title.mp3"}))))

  (testing "the outermost folder is the artist; the immediate parent is the album"
    (is (= {:artist "Music" :album "Album" :title "Title"}
           (tags/from-path {:rel "Music/Rock/Artist/Album/Title.flac"}))))

  (testing "a single folder is taken as the artist, with no album"
    (is (= {:artist "Artist" :album nil :title "Title"}
           (tags/from-path {:rel "Artist/Title.mp3"}))))

  (testing "a bare filename yields only a title"
    (is (= {:artist nil :album nil :title "Title"}
           (tags/from-path {:rel "Title.mp3"}))))

  (testing "falls back to :name when there is no :rel"
    (is (= {:artist nil :album nil :title "Song"}
           (tags/from-path {:name "Song.mp3"}))))

  (testing "only the final extension is stripped; earlier dots are kept"
    (is (= "Song.feat.x" (:title (tags/from-path {:rel "A/B/Song.feat.x.mp3"})))))

  (testing "a name without an extension is kept whole"
    (is (= "Song" (:title (tags/from-path {:rel "A/B/Song"})))))

  (testing "an empty path yields all nils"
    (is (= {:artist nil :album nil :title nil}
           (tags/from-path {:rel ""})))))
