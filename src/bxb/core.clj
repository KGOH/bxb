(ns bxb.core
  (:require [bxb.misc :refer [may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]
            [utiliva.core :refer [keepcat]]))

(defn- walk-path [const-fn search-fn map-fn path cur-path]
  (loop [[first-p & rest-p :as path] path
         cur-path                    cur-path
         walked-paths-vals           '()]
    (cond
      (every? may-be-a-key? path)
      {:walked-path      (into cur-path (map const-fn path))
       :const-paths-vals walked-paths-vals}

      (may-be-a-key? first-p)
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
           (single-elem? first-p))
      (let [ffp (first first-p)]
        (cond
          (map? ffp)
          (recur (into first-p rest-p)
                 (conj cur-path (search-fn cur-path ffp))
                 walked-paths-vals)

          (may-be-a-key? ffp)
          {:const-paths-vals walked-paths-vals
           :map              {:path        rest-p
                              :walked-path (conj cur-path (const-fn ffp))}})))))

(defn- interpret-template [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn src dest cur-src cur-dest]
  (let [{get-value-path :walked-path, dissoc-const-paths-vals :const-paths-vals, {map-src :path, dissoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn map-fn src cur-src)

        {put-value-path :walked-path, assoc-const-paths-vals  :const-paths-vals, {map-dest :path, assoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn map-fn dest cur-dest)]
    (cond
      (and get-value-path put-value-path)
      (concat (map (partial apply assoc-fn) assoc-const-paths-vals)
              [(assoc-fn  put-value-path (get-fn get-value-path))
               (dissoc-fn get-value-path (get-fn get-value-path))]
              (map (partial apply dissoc-fn) dissoc-const-paths-vals))

      (and map-src map-dest)
      [(map-fn dissoc-map-path
               assoc-map-path
               (interpret-template const-fn search-fn map-fn get-fn assoc-fn dissoc-fn map-src map-dest [] []))])))

(defn create-mutations
  "Creates mutations to transmapm data. Bidirectional"
  [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn [from to] template]
  (let [interpret-template*
        (partial interpret-template const-fn search-fn map-fn get-fn assoc-fn dissoc-fn)]
    (keepcat (fn [{src from, dest to}]
               (when (and src dest) (interpret-template* src dest [] [])))
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
