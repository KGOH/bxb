(ns bxb.core
  (:require [bxb.misc :refer [may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk-path [const-fn search-fn ssoc-fn path]
  (loop [mutations []
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? may-be-a-key? path)
      {:mutations mutations
       :path      (into cur-prefix (map const-fn path))}

      (may-be-a-key? first-p)
      (recur mutations
             rest-p
             (conj cur-prefix (const-fn first-p)))

      (map? first-p)
      (recur (conj mutations (ssoc-fn cur-prefix (const-fn first-p)))
             rest-p
             cur-prefix)

      (and (sequential? first-p)
           (single-elem? first-p)
           (map? (first first-p)))
      (recur mutations
             (into first-p rest-p)
             (conj cur-prefix (search-fn cur-prefix (first first-p)))))))

(defn- interpret-template [const-fn search-fn get-fn assoc-fn dissoc-fn src dest]
  (let [{get-value-path :path, dissoc-mutations :mutations} (walk-path const-fn search-fn dissoc-fn src)
        {put-value-path :path, assoc-mutations  :mutations} (walk-path const-fn search-fn assoc-fn dest)]
    (concat
     assoc-mutations
     [(assoc-fn put-value-path (get-fn get-value-path))]
     dissoc-mutations
     [(dissoc-fn get-value-path (get-fn get-value-path))])))

(defn create-mutations
  "Creates mutations to transform data. Bidirectional"
  [const-fn search-fn get-fn assoc-fn dissoc-fn [from to] template]
  (let [interpret-template*
        (partial interpret-template const-fn search-fn get-fn assoc-fn dissoc-fn)]
    (keepcat (fn [{src from, dest to}]
               (when (and src dest) (interpret-template* src dest)))
             template)))

(defn mutate [mutations data]
  (reduce #(%2 %1)
          data
          mutations))

(def hmap-mutations
  (partial create-mutations hmap-const-fn hmap-search-fn hmap-get-fn hmap-assoc-fn hmap-dissoc-fn))

(def sql-mutations
  (partial create-mutations sql-const-fn sql-search-fn sql-get-fn sql-assoc-fn sql-dissoc-fn))

(def test-mutations
  (partial create-mutations
           (fn [& args] (apply list :const-fn args))
           (fn [& args] (apply list :search-fn args))
           (fn [& args] (apply list :get-fn args))
           (fn [& args] (apply list :assoc-fn args))
           (fn [arg & args] (list :dissoc-fn arg))))
