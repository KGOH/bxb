(ns bxb.mutate.hmap
  (:require [bxb.misc :as misc]
            [clojure.data :refer [diff]]
            [bxb.misc :refer [dissoc-in single-elem? assoc-in-vec]]))

(defn match? [m1 m2]
  (= m1 (last (diff m1 m2))))

(defn find-match [pattern s]
  (cond
    (map? s)
    (recur pattern [s])

    (sequential? s)
    (let [results
          (keep-indexed
           (fn [i m] (when (match? pattern m) i))
           s)]
      (cond
        (empty? results)
        (count s)

        (single-elem? results)
        (first results)

        :else
        (throw (Exception. "there is more than one match in vector"))))

    :else
    0))

(defn resolve-path [path data-source]
  (mapv (fn [pt] (pt data-source))
        path))

(defn const-fn [constant]
  (constantly constant))

(defn get-fn [path]
  (fn [data-source] (get-in data-source (resolve-path path data-source))))

(defn search-fn [path value]
  (fn [data-source]
    (let [get-value (get-fn path)]
      (find-match value (get-value data-source)))))

(defn map-fn [src-path dest-path mutations]
  (fn [data-source]
    (let [dest (resolve-path dest-path data-source)
          src  (resolve-path src-path  data-source)
          mut-fn (partial misc/mutate mutations)]
      (if-let [data (get-in data-source src)]
        (-> data-source
            (assoc-in dest (mapv mut-fn ;TODO: call bxb.core/mutate instead of creating the same f
                                 (get-in data-source src)))
            (cond->
              (not= src dest)
              (dissoc-in src)))
        data-source))))

(defn assoc-fn [path get-value]
  (fn [data-source]
    (let [p (resolve-path path data-source)]
      (if (not-any? nil? p)
        (assoc-in-vec data-source p (get-value data-source))
        data-source))))

(defn dissoc-fn
  ([path]
   (fn [data-source]
     (let [p (resolve-path path data-source)]
       (if (not-any? nil? p)
         (dissoc-in data-source p)
         data-source))))
  ([path value] ; TODO: match dissocing value with provided value
   (dissoc-fn path)))

(def fns
  {:const-fn  const-fn
   :search-fn search-fn
   :map-fn    map-fn
   :get-fn    get-fn
   :assoc-fn  assoc-fn
   :dissoc-fn dissoc-fn})
