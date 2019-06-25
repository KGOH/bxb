(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk [search-fn soc-fn path]
  (loop [mutations []
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? may-be-a-key? path)
      {:path  (concat cur-prefix (map constantly path))
       :mutations mutations}

      (may-be-a-key? first-p)
      (recur mutations
             rest-p
             (conj cur-prefix (constantly first-p)))

      (map? first-p)
      (recur (conj mutations (soc-fn cur-prefix (constantly first-p)))
             rest-p
             cur-prefix)

      (and (sequential? first-p)
           (single-elem? first-p)
           (map? (first first-p)))
      (recur mutations
             (into first-p rest-p)
             (conj cur-prefix (search-fn cur-prefix (first first-p)))))))

(defn- walk-path [search-fn get-fn assoc-fn dissoc-fn src dest]
  (let [{get-value-path :path, dissoc-mutations :mutations} (walk search-fn dissoc-fn src)
        {put-value-path :path, assoc-mutations  :mutations} (walk search-fn assoc-fn  dest)]
    (-> assoc-mutations
        (conj (assoc-fn put-value-path (get-fn get-value-path)))
        (conj (dissoc-fn get-value-path))
        (into dissoc-mutations))))

(defn create-mutations
  "Creates mutations to transform data. Bidirectional"
  [search-fn get-fn assoc-fn dissoc-fn [from to] template]
  (keepcat (fn [{src from, dest to}]
             (when (and src dest) (walk-path search-fn get-fn assoc-fn dissoc-fn src dest)))
           template))

(defn mutate [mutations data]
  (reduce #(%2 %1)
          data
          mutations))

(defn hmap-mutations [dir template]
  (create-mutations hmap-search-fn hmap-get-fn hmap-assoc-fn hmap-dissoc-fn dir template))

(defn sql-mutations [dir template]
  (create-mutations sql-search-fn sql-get-fn sql-assoc-fn sql-dissoc-fn dir template))
