(ns dapr.domain.plan
  "Pure computation of a selective sync plan: make the sink hold exactly the
  selected tracks, via add / move / delete / skip actions. No I/O.

  Input map:
    :source-catalog  key -> track (from the source library)
    :sink-catalog    key -> track (from the sink library)
    :selected        set of selected track keys
    :sink-roots      ordered [{:uri :free-bytes}] for add placement

  Track identity is [rel size] (root-excluded), so a track present on the sink at
  the same relative path is the same track regardless of which sink device holds
  it (-> :skip). A selected track with no match on the sink is added; an on-sink
  track that is not selected is deleted. (There is no move op: with rel-based
  identity a file moved to a new relative path is simply a delete of the old path
  plus an add of the new one.)

  Placement: a newly added track keeps its source-relative subpath (:rel) and is
  written under the first sink root whose remaining free space fits it; remaining
  free is threaded through placement so successive adds account for earlier ones.
  An add that fits no single root becomes a :blocked action.")

(defn- place
  "Choose a target {:root :rel} for adding `track` given `roots` (ordered) and
  `free` (a vector of remaining free bytes, parallel to roots). Returns
  [target updated-free], or [nil free] when no root fits."
  [track roots free]
  (let [n (count roots)]
    (loop [i 0]
      (cond
        (>= i n) [nil free]
        (>= (nth free i) (:size track))
        [{:root (:uri (nth roots i)) :rel (:rel track)}
         (update free i - (:size track))]
        :else (recur (inc i))))))

(defn- selected-step
  [roots {:keys [acc free]} src snk k]
  (cond
    snk
    {:acc (conj acc {:op :skip :key k}) :free free}

    :else
    (let [[target free'] (place src roots free)]
      (if target
        {:acc  (conj acc {:op     :add :key k
                          :src    {:root (:root src) :rel (:rel src)}
                          :target target
                          :size   (:size src)})
         :free free'}
        {:acc  (conj acc {:op :blocked :key k :reason :no-room :size (:size src)})
         :free free}))))

(defn selection-plan
  "Compute the ordered vector of actions that makes the sink hold exactly the
  selected tracks. See the namespace docstring for the input map and placement."
  [{:keys [source-catalog sink-catalog selected sink-roots]}]
  (let [roots   (vec sink-roots)
        sel     (sort (filter #(contains? source-catalog %) selected))
        placed  (reduce (fn [state k]
                          (selected-step roots state
                                         (get source-catalog k)
                                         (get sink-catalog k)
                                         k))
                        {:acc [] :free (mapv :free-bytes roots)}
                        sel)
        deletes (sort-by :key
                         (for [[k t] sink-catalog
                               :when (not (contains? selected k))]
                           {:op :delete :key k
                            :at {:root (:root t) :rel (:rel t)} :size (:size t)}))]
    (vec (concat (:acc placed) deletes))))

(defn summary
  "Summarize a plan: op counts and bytes added/freed."
  [plan]
  (let [by-op (group-by :op plan)
        bytes (fn [op] (reduce + 0 (map :size (get by-op op))))]
    {:add         (count (:add by-op))
     :delete      (count (:delete by-op))
     :skip        (count (:skip by-op))
     :blocked     (count (:blocked by-op))
     :bytes-added (bytes :add)
     :bytes-freed (bytes :delete)}))
