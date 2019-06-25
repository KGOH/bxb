(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk-pop-in [get-fn dissoc-fn src]
  (loop [mutations []
         [first-s & rest-s :as src] src
         cur-prefix []]
    (cond
      (every? may-be-a-key? src)
      (let [p (concat cur-prefix src)
            get-value (get-fn p)
            pop-value (dissoc-fn p)]
        {:get-value get-value
         :dissoc-mutations (conj mutations pop-value)})

      (may-be-a-key? first-s)
      (recur mutations
             rest-s
             (conj cur-prefix first-s))

      (map? first-s)
      (recur (into mutations (map (fn [k] (dissoc-fn (conj cur-prefix k))) (keys first-s)))
             rest-s
             cur-prefix)

      (and (sequential? first-s) ; TODO: add search
           (single-elem? first-s)
           (map? (first first-s)))
      (recur (into mutations (map (fn [k] (dissoc-fn (conj cur-prefix 0 k))) (keys first-s)))
             rest-s
             cur-prefix))))

(defn- walk-put-in [assoc-fn dest get-value]
  (loop [mutations []
         [first-d & rest-d :as dest] dest
         cur-prefix []]
    (cond
      (every? may-be-a-key? dest)
      (conj mutations
            (assoc-fn (concat cur-prefix dest) get-value))

      (may-be-a-key? first-d)
      (recur mutations
             rest-d
             (conj cur-prefix first-d))

      (map? first-d)
      (recur (conj mutations (assoc-fn cur-prefix (constantly first-d)))
             rest-d
             cur-prefix)

      (and (sequential? first-d) ; TODO: add upsert
           (single-elem? first-d)
           (map? (first first-d)))
      (recur (conj mutations (assoc-fn cur-prefix (constantly first-d)))
             rest-d
             (conj cur-prefix 0)))))

(defn- walk-path [get-fn assoc-fn dissoc-fn src dest]
  (let [{:keys [get-value dissoc-mutations]} (walk-pop-in get-fn dissoc-fn src)
        assoc-mutations (walk-put-in assoc-fn dest get-value)]
    (into assoc-mutations dissoc-mutations)))

(defn create-mutations*
  "Creates mutations to transform data. Bidirectional"
  [get-fn assoc-fn dissoc-fn [from to] template]
  (keepcat (fn [{src from, dest to}]
             (when (and src dest) (walk-path get-fn assoc-fn dissoc-fn src dest)))
           template))

(defn mutate [mutations data]
  (reduce #(%2 %1)
          data
          mutations))

(defn hmap-mutations [dir template]
  (create-mutations* hmap-get-fn hmap-assoc-fn hmap-dissoc-fn dir template))

(defn sql-mutations [dir template]
  (create-mutations* sql-get-fn sql-assoc-fn sql-dissoc-fn dir template))
