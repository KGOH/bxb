(ns bxb.misc)

(defn assoc-in-vec [m [k & ks] v]
  (cond
    (nil? v)
    m

    (and (nil? m)
         (integer? k)
         (zero? k))
    (if ks
      [(assoc-in-vec (get m k) ks v)]
      [v])

    (or (map? m)
        (sequential? m)
        (and (nil? m) (keyword? k)))
    (if ks
      (assoc m k (assoc-in-vec (get m k) ks v))
      (assoc m k v))))

(defn vec-remove
  "remove elem in coll
   Credit: https://stackoverflow.com/a/18319708/6179231"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn single-elem? [s]
  (and (seq s)
       (empty? (rest s))))

(defn may-be-a-key? [x]
  (or (keyword? x)
      (integer? x)
      (string?  x)))

(defmacro p [data]
  `(do ;(println (str ~(resolve data) \:))
     (clojure.pprint/pprint ~data)
     ~data))

(defn dissocv [m & ks]
  (cond
    (map? m)
    (apply dissoc m ks)

    (sequential? m)
    (reduce vec-remove (vec m) ks)))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure.
  Credit: https://stackoverflow.com/a/14488425/6179231"
  ([m [k & ks :as keys]]
   (if ks
     (if-let [nextmap (get m k)]
       (let [newmap (dissoc-in nextmap ks)]
         (if (seq newmap)
           (assoc m k newmap)
           (dissocv m k)))
       m)
     (dissocv m k))))
