(ns bxb.mutate-fns
  (:require [clojure.string :as str]
            [bxb.misc :refer [dissoc-in p single-elem? assoc-in-vec]]
            [cheshire.core :as json]
            [clojure.data :refer [diff]]))

(defn match? [m1 m2]
  (= m1 (last (diff m1 m2))))

(defn find-match [pattern s]
  (if (map? s)
    (recur pattern [s])
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
        (throw (Exception. "there is more than one match in vector"))))))

(defn resolve-path [path data-source]
  (mapv (fn [pt] (pt data-source))
        path))

(defn hmap-get-fn [path]
  (fn [data-source] (get-in data-source (resolve-path path data-source))))

(defn hmap-search-fn [path value]
  (fn [data-source]
    (let [get-value (hmap-get-fn path)]
      (find-match value (get-value data-source)))))

(defn hmap-assoc-fn [path get-value]
  (fn [data-source] (assoc-in-vec data-source (resolve-path path data-source) (get-value data-source))))

(defn hmap-dissoc-fn [path]
  (fn [data-source] (dissoc-in data-source (resolve-path path data-source))))

(defn kvs->jsidx [s]
  (str "'{" (str/join \, (map json/generate-string s)) "}'"))

(defn sql-search-fn [path value]
  (fn [data-source] (str "SELECT " (resolve-path path data-source) " FROM " data-source " WHERE " value)));(constantly 0))

(defn sql-get-fn [path]
  (fn [data-source] (str data-source "#>>" (kvs->jsidx (resolve-path path data-source)))))

(defn sql-assoc-fn [path get-value]
  (fn [data-source] (str "|| jsonb_set(" data-source ", " (kvs->jsidx (resolve-path path data-source)) ", " (get-value data-source) ")")))

(defn sql-dissoc-fn [path]
  (fn [data-source] (str "#- " (kvs->jsidx (resolve-path path data-source)))))
