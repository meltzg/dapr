(ns dapr.cache
  "Persisted scan cache and system of record for libraries, backed by an
  in-memory DataScript database snapshotted to an EDN file. The DB owns library
  identity and references: a *library* entity holds its name and roots; a *track*
  entity is the device-independent identity [rel size] plus artist/album/title
  tags; and a *presence* links a track to the library it was found on, recording
  the root it lives under and its mtime. Tracks/presences are derived (a rescan
  rebuilds them), but libraries are authoritative user config, so writes are
  atomic and a corrupt/old snapshot is preserved rather than silently discarded.

  Query fns take a `db` value and are pure; transaction and file fns take a
  `conn` (or path) and end in `!`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [dapr.fs.paths :as paths])
  (:import (java.io File)
           (java.nio.file CopyOption Files StandardCopyOption)))

(def schema
  "DataScript schema. Only refs and the composite-identity tuples need
  declaring; scalar attributes are schemaless."
  {:track/key        {:db/tupleAttrs [:track/rel :track/size]
                      :db/unique     :db.unique/identity}
   :presence/library {:db/valueType :db.type/ref}
   :presence/track   {:db/valueType :db.type/ref}
   :presence/key     {:db/tupleAttrs [:presence/library :presence/track]
                      :db/unique     :db.unique/identity}})

(def snapshot-version
  "Bumped when the on-disk snapshot shape changes; an older/garbled snapshot is
  backed up and the DB starts empty (libraries re-import from libraries.edn)."
  1)

;; --- file paths --------------------------------------------------------------

(defn default-path!
  "OS-appropriate path to cache.edn under the user's config directory
  ($XDG_CONFIG_HOME, %APPDATA%, or ~/.config)."
  ^File []
  (let [base (or (System/getenv "XDG_CONFIG_HOME")
                 (System/getenv "APPDATA")
                 (io/file (paths/user-home) ".config"))]
    (io/file base "dapr" "cache.edn")))

;; --- load / snapshot ---------------------------------------------------------

(defn empty-conn
  "A fresh connection with the cache schema and no data."
  []
  (d/create-conn schema))

(defn- backup-corrupt!
  "Move an unreadable/old snapshot aside so it isn't overwritten, preserving any
  authoritative library data for manual recovery."
  [^File f]
  (let [dst (io/file (str (.getPath f) ".corrupt-" (System/currentTimeMillis)))]
    (.renameTo f dst)
    dst))

(defn load!
  "Read the snapshot at `path` into a connection. A missing file yields an empty
  DB; an unreadable file or a version mismatch is backed up (see backup-corrupt!)
  and also yields an empty DB."
  [path]
  (let [f (io/file path)]
    (if-not (.exists f)
      (empty-conn)
      (try
        (let [{:keys [version db]} (edn/read-string (slurp f))]
          (if (= version snapshot-version)
            (d/conn-from-db (d/from-serializable db))
            (do (backup-corrupt! f) (empty-conn))))
        (catch Exception _
          (backup-corrupt! f)
          (empty-conn))))))

(defn snapshot!
  "Atomically write `conn`'s DB to `path` as versioned EDN (temp file + move), so
  a crash mid-write can't corrupt an existing snapshot."
  [conn path]
  (let [f   (io/file path)
        tmp (io/file (str (.getPath f) ".tmp"))]
    (io/make-parents f)
    (spit tmp (pr-str {:version snapshot-version :db (d/serializable @conn)}))
    (Files/move (.toPath tmp) (.toPath f)
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
    path))

;; --- libraries ---------------------------------------------------------------

(defn libraries
  "All libraries as UI projection maps {:id :name :roots}, in creation order
  (ascending entity id). :id is the DataScript entity id."
  [db]
  (->> (d/q '[:find [(pull ?e [:db/id :library/name :library/roots]) ...]
              :where [?e :library/name]]
            db)
       (map (fn [m] {:id    (:db/id m)
                     :name  (:library/name m)
                     :roots (vec (:library/roots m))}))
       (sort-by :id)
       (vec)))

(defn upsert-library!
  "Create or update a library from {:id <eid|nil> :name :roots}. Returns its
  entity id."
  [conn {:keys [id name roots]}]
  (let [tempid (or id "new-library")
        report (d/transact! conn [{:db/id         tempid
                                   :library/name  name
                                   :library/roots (vec roots)}])]
    (or id (get (:tempids report) tempid))))

(defn- library-presences
  "Entity ids of every presence belonging to library `lib-eid`."
  [db lib-eid]
  (d/q '[:find [?p ...] :in $ ?lib :where [?p :presence/library ?lib]] db lib-eid))

(defn delete-library!
  "Retract library `lib-eid` and all presences that referenced it."
  [conn lib-eid]
  (d/transact! conn (into [[:db/retractEntity lib-eid]]
                          (map (fn [p] [:db/retractEntity p]))
                          (library-presences (d/db conn) lib-eid))))

(defn migrate-from-edn!
  "One-time import of legacy libraries (the vector from libraries.edn) into an
  empty DB, dropping their old string ids so the DB assigns fresh entity ids."
  [conn legacy-libraries]
  (doseq [lib legacy-libraries]
    (upsert-library! conn (-> lib (select-keys [:name :roots]) (assoc :id nil)))))

;; --- catalog queries ---------------------------------------------------------

(defn library-catalog
  "key -> track map for library `lib-eid`, in the shape the planner and table
  expect: {:key [rel size] :rel :size :root :mtime :artist :album :title}. Missing
  tags/mtime come back as nil."
  [db lib-eid]
  (->> (d/q '[:find [(pull ?p [:presence/root :presence/mtime
                               {:presence/track [:track/rel :track/size
                                                 :track/artist :track/album :track/title]}]) ...]
              :in $ ?lib
              :where [?p :presence/library ?lib]]
            db lib-eid)
       (reduce (fn [acc {:keys [presence/root presence/mtime presence/track]}]
                 (let [{:keys [track/rel track/size track/artist track/album track/title]} track]
                   (assoc acc [rel size]
                          {:key   [rel size] :rel rel :size size :root root :mtime mtime
                           :artist artist :album album :title title})))
               {})))

(defn track-libraries
  "Entity ids of the libraries that hold track [rel size]."
  [db rel size]
  (d/q '[:find [?lib ...] :in $ ?rel ?size
         :where [?t :track/rel ?rel] [?t :track/size ?size]
         [?p :presence/track ?t] [?p :presence/library ?lib]]
       db rel size))

;; --- tracks & presences ------------------------------------------------------

(defn- track-tx
  "tx-data upserting `track` (by its [rel size] identity) and a presence linking
  it to library `lib-eid`. Nil tags are omitted (DataScript rejects nil values)."
  [lib-eid {:keys [rel size artist album title root mtime]}]
  (let [tid (str "track-" rel "-" size)]
    [(cond-> {:db/id tid :track/rel rel :track/size size}
       artist (assoc :track/artist artist)
       album  (assoc :track/album album)
       title  (assoc :track/title title))
     (cond-> {:presence/library lib-eid :presence/track tid :presence/root root}
       mtime (assoc :presence/mtime mtime))]))

(def ^:private tx-batch-size
  "Tracks per transaction in replace-library-tracks!. DataScript resolves each
  upserting tempid by recursing through the rest of the transaction (retry-with-
  tempid -> transact-tx-data-impl), so a single transaction upserting a whole
  library re-scan recurses once per track and overflows the stack. Batching keeps
  that recursion bounded (~2x this, for the track + its presence)."
  256)

(defn- track-changed?
  "True when scanned track `t` differs from its `cached` counterpart (same [rel
  size]) in anything the cache stores — its tags, the root it lives under, or its
  mtime — so an unchanged track needn't be re-transacted."
  [cached t]
  (or (not= (:mtime cached) (:mtime t))
      (not= (:root cached) (:root t))
      (not= (:artist cached) (:artist t))
      (not= (:album cached) (:album t))
      (not= (:title cached) (:title t))))

(defn replace-library-tracks!
  "Set library `lib-eid`'s presences to exactly `tracks` (catalog track maps).
  Diffs against the library's current cached catalog (`cached`, key -> track —
  queried when not supplied) and only transacts the delta: retract presences whose
  track is gone, and upsert tracks that are new or changed (see track-changed?).
  An unchanged re-scan therefore transacts nothing. Upserts are batched so a large
  change set can't overflow DataScript's per-upsert recursion (see tx-batch-size)."
  ([conn lib-eid tracks]
   (replace-library-tracks! conn lib-eid tracks (library-catalog (d/db conn) lib-eid)))
  ([conn lib-eid tracks cached]
   (let [want     (set (map (juxt :rel :size) tracks))
         existing (d/q '[:find ?p ?rel ?size :in $ ?lib
                         :where [?p :presence/library ?lib]
                         [?p :presence/track ?t]
                         [?t :track/rel ?rel] [?t :track/size ?size]]
                       (d/db conn) lib-eid)
         retract  (vec (for [[p rel size] existing
                             :when (not (want [rel size]))]
                         [:db/retractEntity p]))
         changed  (filter (fn [t]
                            (let [c (cached [(:rel t) (:size t)])]
                              (or (nil? c) (track-changed? c t))))
                          tracks)]
     (when (seq retract)
       (d/transact! conn retract))
     (doseq [batch (partition-all tx-batch-size changed)]
       (d/transact! conn (into [] (mapcat #(track-tx lib-eid %)) batch))))))

(defn add-presence!
  "Record that `track` is now on library `lib-eid` (used after a sync add)."
  [conn lib-eid track]
  (d/transact! conn (track-tx lib-eid track)))

(defn remove-presence!
  "Drop the presence of track [rel size] on library `lib-eid` (used after a sync
  delete). The track entity is left in place; other libraries may still hold it."
  [conn lib-eid rel size]
  (when-let [p (d/q '[:find ?p . :in $ ?lib ?rel ?size
                      :where [?p :presence/library ?lib]
                      [?p :presence/track ?t]
                      [?t :track/rel ?rel] [?t :track/size ?size]]
                    (d/db conn) lib-eid rel size)]
    (d/transact! conn [[:db/retractEntity p]])))
