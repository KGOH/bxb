(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk-pop-in [search-fn get-fn dissoc-fn src]
  (loop [mutations []
         [first-s & rest-s :as src] src
         cur-prefix []]
    (cond
      (every? may-be-a-key? src)
      (let [p (concat cur-prefix (map constantly src))
            get-value (get-fn p)
            pop-value (dissoc-fn p)]
        {:get-value get-value
         :dissoc-mutations (conj mutations pop-value)})

      (may-be-a-key? first-s)
      (recur mutations
             rest-s
             (conj cur-prefix (constantly first-s)))

      (map? first-s)
      (recur (conj mutations (dissoc-fn cur-prefix))
             rest-s
             cur-prefix)

      (and (sequential? first-s)
           (single-elem? first-s)
           (map? (first first-s)))
      (recur mutations
             (into first-s rest-s)
             (conj cur-prefix (search-fn cur-prefix (first first-s)))))))

(defn- walk-put-in [search-fn assoc-fn dest get-value]
  (loop [mutations []
         [first-d & rest-d :as dest] dest
         cur-prefix []]
    (cond
      (every? may-be-a-key? dest)
      (conj mutations
            (assoc-fn (concat cur-prefix (map constantly dest)) get-value))

      (may-be-a-key? first-d)
      (recur mutations
             rest-d
             (conj cur-prefix (constantly first-d)))

      (map? first-d)
      (recur (conj mutations (assoc-fn cur-prefix (constantly first-d)))
             rest-d
             cur-prefix)

      (and (sequential? first-d)
           (single-elem? first-d)
           (map? (first first-d)))
      (recur mutations
             (into first-d rest-d)
             (conj cur-prefix (search-fn cur-prefix (first first-d)))))))

(defn- walk-path [search-fn get-fn assoc-fn dissoc-fn src dest]
  (let [{:keys [get-value dissoc-mutations]} (walk-pop-in search-fn get-fn dissoc-fn src)
        assoc-mutations (walk-put-in search-fn assoc-fn dest get-value)]
    (into assoc-mutations dissoc-mutations)))

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
