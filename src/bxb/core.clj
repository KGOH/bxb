(ns bxb.core
  (:require [bxb.misc :refer [may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk-path [search-fn ssoc-fn path]
  (loop [walked-paths []
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? may-be-a-key? path)
      {:mutations walked-paths
       :path      (concat cur-prefix (map constantly path))}

      (may-be-a-key? first-p)
      (recur walked-paths
             rest-p
             (conj cur-prefix (constantly first-p)))

      (map? first-p)
      (recur (conj walked-paths (ssoc-fn cur-prefix (constantly first-p)))
             rest-p
             cur-prefix)

      (and (sequential? first-p)
           (single-elem? first-p)
           (map? (first first-p)))
      (recur walked-paths
             (into first-p rest-p)
             (conj cur-prefix (search-fn cur-prefix (first first-p)))))))

(defn- interpret-template [search-fn get-fn assoc-fn dissoc-fn src dest]
  (let [{get-value-path :path, dissoc-mutations :mutations} (walk-path search-fn dissoc-fn src)
        {put-value-path :path, assoc-mutations  :mutations} (walk-path search-fn assoc-fn dest)]
    (concat
      assoc-mutations
      [(assoc-fn put-value-path (get-fn get-value-path))]
      dissoc-mutations
      [(dissoc-fn get-value-path (get-fn get-value-path))])))

(defn create-mutations
  "Creates mutations to transform data. Bidirectional"
  [search-fn get-fn assoc-fn dissoc-fn [from to] template]
  (let [interpret-template*
        (partial interpret-template search-fn get-fn assoc-fn dissoc-fn)]
    (keepcat (fn [{src from, dest to}]
               (when (and src dest) (interpret-template* src dest)))
             template)))

(defn mutate [mutations data]
  (reduce #(%2 %1)
          data
          mutations))

(def hmap-mutations
  (partial create-mutations hmap-search-fn hmap-get-fn hmap-assoc-fn hmap-dissoc-fn))

(def sql-mutations
  (partial create-mutations sql-search-fn sql-get-fn sql-assoc-fn sql-dissoc-fn))
