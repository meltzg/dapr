(ns dapr.sync
  "Side-effecting execution of a selective library sync. The plan is computed by
  the pure dapr.domain.plan; this namespace scans libraries, queries capacity,
  and performs the add/move/delete operations (all fns end in !)."
  (:require [dapr.domain.library :as lib]
            [dapr.domain.plan :as plan]
            [dapr.fs.nio :as nio]))

(defn catalog-of!
  "Scan a library's roots into a catalog (key -> track). When `on-scan` is
  supplied it receives per-directory and per-file scan events as they happen."
  ([library] (catalog-of! library nil))
  ([{:keys [roots]} on-scan]
   (lib/catalog (nio/catalog! roots on-scan))))

(defn sink-roots!
  "Per-root free space for a sink library, in library order (placement input)."
  [{:keys [roots]}]
  (mapv nio/root-free! roots))

(defn library-free!
  "Total usable bytes across the distinct devices backing a library's roots."
  [{:keys [roots]}]
  (nio/library-free! roots))

(defn build-plan!
  "Scan source + sink and compute the selection plan for the `selected` keys. The
  two scans run concurrently (melt-jfs serializes per device, so this is a real
  speedup whenever they live on different devices, and harmless otherwise). The
  optional `on-source` / `on-sink` callbacks receive per-directory and per-file
  scan events from the respective library's walk."
  ([source-lib sink-lib selected] (build-plan! source-lib sink-lib selected nil))
  ([source-lib sink-lib selected {:keys [on-source on-sink]}]
   (let [source-fut (future (catalog-of! source-lib on-source))
         sink-cat   (catalog-of! sink-lib on-sink)]
     (plan/selection-plan
      {:source-catalog @source-fut
       :sink-catalog   sink-cat
       :selected       selected
       :sink-roots     (sink-roots! sink-lib)}))))

(defn execute-plan!
  "Execute plan `actions` against the sink: copies and deletes flow through
  dapr.fs.nio. :skip and :blocked actions are ignored. When supplied, calls
  (on-progress {:done n :total t :action a}) after each performed action.
  Returns {:add n :delete n}."
  ([actions] (execute-plan! actions nil))
  ([actions {:keys [on-progress]}]
   (let [resolve-root (memoize nio/root-path!)
         todo  (remove (comp #{:skip :blocked} :op) actions)
         total (count todo)]
     (reduce
      (fn [acc [i a]]
        (case (:op a)
          :add    (nio/copy-file! (resolve-root (get-in a [:src :root]))
                                  (resolve-root (get-in a [:target :root]))
                                  (get-in a [:src :rel]))
          :delete (nio/delete-file! (resolve-root (get-in a [:at :root]))
                                    (get-in a [:at :rel])))
        (when on-progress
          (on-progress {:done (inc i) :total total :action a}))
        (update acc (:op a) (fnil inc 0)))
      {:add 0 :delete 0}
      (map-indexed vector todo)))))
