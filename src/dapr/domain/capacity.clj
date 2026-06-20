(ns dapr.domain.capacity
  "Pure capacity math for a sink library. The side-effecting free-space query
  lives in dapr.fs.nio; here we only combine numbers and catalogs."
  (:require [dapr.domain.library :as lib]))

(defn budget
  "Maximum total size the sink can hold for the selected set: free space across
  the sink's distinct devices plus the bytes already occupied by tracks on the
  sink (each of which is either kept — and thus counted in the selection — or
  deleted, reclaiming its space)."
  [free-bytes sink-catalog]
  (+ (or free-bytes 0) (lib/track-total-size (vals sink-catalog))))

(defn- track-size
  [k source-catalog sink-catalog]
  (:size (or (get source-catalog k) (get sink-catalog k)) 0))

(defn used
  "Total size of the currently selected tracks."
  [selected source-catalog sink-catalog]
  (reduce + 0 (map #(track-size % source-catalog sink-catalog) selected)))

(defn usage
  "Capacity snapshot: {:used :budget :free} for the current selection."
  [selected source-catalog sink-catalog free-bytes]
  (let [b (budget free-bytes sink-catalog)
        u (used selected source-catalog sink-catalog)]
    {:used u :budget b :free (- b u)}))

(defn would-fit?
  "True when selecting key `k` keeps the selected total within budget. A key
  already selected, or already on the sink, never adds new bytes and always
  fits."
  [k selected source-catalog sink-catalog free-bytes]
  (or (contains? selected k)
      (contains? sink-catalog k)
      (<= (+ (used selected source-catalog sink-catalog)
             (track-size k source-catalog sink-catalog))
          (budget free-bytes sink-catalog))))
