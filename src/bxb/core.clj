(ns bxb.core
  (:require [bxb.misc         :as misc]
            [bxb.mutate.hmap  :as hmap]
            [bxb.mutate.sql   :as sql]
            [bxb.mutate.debug :as debug]
            [honeysql.core    :as hsql]
            [bxb.honey]
            [spyscope.core]))

(defn- walk-path [const-fn search-fn path]
  (loop [[first-p & rest-p :as path] path
         cur-path                    []
         walked-paths-vals           '()]
    (cond
      (every? misc/key? path)
      {:walked-path      (into cur-path (map const-fn path))
       :const-paths-vals walked-paths-vals}

      (misc/key? first-p)
      (recur rest-p
             (conj cur-path (const-fn first-p))
             walked-paths-vals)

      (map? first-p)
      (recur rest-p
             cur-path
             (concat (map (fn [[k v]] [(conj cur-path (const-fn k)) (const-fn v)])
                          first-p)
                     walked-paths-vals))

      (and (sequential? first-p)
           (misc/single-elem? first-p))
      (let [[ffp] first-p]
        (cond
          (map? ffp)
          (recur (into first-p rest-p)
                 (conj cur-path (search-fn cur-path ffp))
                 walked-paths-vals)

          (misc/key? ffp)
          {:const-paths-vals walked-paths-vals
           :map              {:path        rest-p
                              :walked-path (conj cur-path (const-fn ffp))}})))))

(defn- dissoc? [a b]
  (some false? (map = a b)))

(defn- interpret-mapping [{:keys [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn] :as fns} src dest]
  (let [{get-value-path :walked-path, dissoc-const-paths-vals :const-paths-vals, {map-src :path, dissoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn src)

        {put-value-path :walked-path, assoc-const-paths-vals  :const-paths-vals, {map-dest :path, assoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn dest)]
    (cond
      (and get-value-path put-value-path)
      (concat (map (partial apply assoc-fn) assoc-const-paths-vals)
              [(assoc-fn  put-value-path (get-fn get-value-path))]
              (when (dissoc? src dest)
                [(dissoc-fn get-value-path (get-fn get-value-path))])
              (map (partial apply dissoc-fn) dissoc-const-paths-vals))

      (and map-src map-dest)
      [(map-fn dissoc-map-path
               assoc-map-path
               (interpret-mapping fns map-src map-dest))])))

(defn create-transformations
  "Creates mutations to transmapm data. Bidirectional"
  [fns [from to] mapping]
  (misc/keepcat (fn [{src from, dest to}]
                  (when (and src dest) (interpret-mapping fns src dest)))
                mapping))

(def debug-transformations (partial create-transformations debug/fns))
(def hmap-transformations  (partial create-transformations hmap/fns))
(def sql-transformations   (partial create-transformations sql/fns))

(def mutate misc/mutate)

(comment
  (let [{:keys [v1 v2 mapping]}
        {:v1      {:resourceType "type"
                   :a            1}
         :v2      {:resourceType "type"
                   :b            1}
                                        ;{"a": {"c": [{"f": 2}], "d": 1, "e": -1}}
         :mapping [{:v1 [:a :d]
                    :v2 [:d]}
                   {:v1 [:a :e]
                    :v2 [:b :c]}
                   {:v1 [:a :c 0 :f]
                    :v2 [:a :c :f]}]}]
    (debug-transformations [:v1 :v2] mapping)
    (-> #_{:update :set_test
         :set {:resource (mutate (sql-transformations [:v1 :v2] mapping) :resource)}
         :returning [:*]}
        {:select [(mutate (sql-transformations [:v1 :v2] mapping) :resource)]
         :from [:set_test]}
        hsql/format
        misc/hsql-subs
        println))

  nil)
