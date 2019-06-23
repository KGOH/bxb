(ns bxb.misc)

(defn dissocv [m & ks]
  (cond
    (map? m)
    (apply dissoc m ks)

    :else
    (let [v (apply assoc m (interleave ks (repeat nil)))]
      (if (seq (remove nil? v))
        v
        []))))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure.
  Credit: https://stackoverflow.com/a/14488425/6179231"
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissocv m k)))
      m)
    (dissocv m k)))
