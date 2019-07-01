(ns bxb.core
  (:require [bxb.misc :refer [may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk-path [const-fn search-fn map-fn path]
  (loop [[first-p & rest-p :as path] path
         cur-path                    []
         walked-paths-vals           '()]
    (cond
      (every? may-be-a-key? path)
      (conj walked-paths-vals (into cur-path (map const-fn path)))

      (may-be-a-key? first-p)
      (recur rest-p
             (conj cur-path (const-fn first-p))
             walked-paths-vals)

      (map? first-p)
      (let [const-paths
            (map (fn [[k v]] [(conj cur-path (const-fn k)) (const-fn v)])
                 first-p)]
        (recur rest-p
               cur-path
               (concat const-paths walked-paths-vals)))

      (and (sequential? first-p)
           (single-elem? first-p))
      (let [ffp (first first-p)]
        (cond
          (map? ffp)
          (recur (into first-p rest-p)
                 (conj cur-path (search-fn cur-path ffp))
                 walked-paths-vals))))))

(defn- interpret-template [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn src dest]
  (let [[get-value-path & dissoc-const-paths-vals] (walk-path const-fn search-fn map-fn src)
        [put-value-path &  assoc-const-paths-vals] (walk-path const-fn search-fn map-fn dest)
        get-value (get-fn get-value-path)]
    (into (map (partial apply assoc-fn)  (conj assoc-const-paths-vals  [put-value-path get-value]))
          (map (partial apply dissoc-fn) (conj dissoc-const-paths-vals [get-value-path get-value])))))

(defn create-mutations
  "Creates mutations to transmapm data. Bidirectional"
  [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn [from to] template]
  (let [interpret-template*
        (partial interpret-template const-fn search-fn map-fn get-fn assoc-fn dissoc-fn)]
    (keepcat (fn [{src from, dest to}]
               (when (and src dest) (interpret-template* src dest)))
             template)))

(defn mutate [mutations data]
  (reduce #(%2 %1)
          data
          mutations))

(def hmap-mutations
  (partial create-mutations hmap-const-fn hmap-search-fn hmap-map-fn hmap-get-fn hmap-assoc-fn hmap-dissoc-fn))

(def sql-mutations
  (partial create-mutations sql-const-fn sql-search-fn sql-map-fn sql-get-fn sql-assoc-fn sql-dissoc-fn))

(def test-mutations
  (partial create-mutations
           (fn [& args] (apply list :const-fn args))
           (fn [& args] (apply list :search-fn args))
           (fn [& args] (apply list :map-fn args))
           (fn [& args] (apply list :get-fn args))
           (fn [& args] (apply list :assoc-fn args))
           (fn [arg & args] (list :dissoc-fn arg))))
