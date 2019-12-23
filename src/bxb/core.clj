(ns bxb.core
  (:require [bxb.misc :refer [may-be-a-key? single-elem?]]
            [bxb.mutate-fns :as mf]
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

(defn- interpret-template [{:keys [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn] :as fns} src dest cur-src cur-dest]
  (let [{get-value-path :walked-path, dissoc-const-paths-vals :const-paths-vals, {map-src :path, dissoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn map-fn src cur-src)

        {put-value-path :walked-path, assoc-const-paths-vals  :const-paths-vals, {map-dest :path, assoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn map-fn dest cur-dest)

        bxb_get_buffer
        [(const-fn :bxb_get_buffer)]]  ;; Maybe need another way to avoid dissocing the same key where new data was assoced?
    (cond
      (and get-value-path put-value-path)
      (concat [(assoc-fn bxb_get_buffer (get-fn get-value-path))]
              (map (partial apply assoc-fn) assoc-const-paths-vals)
              [(assoc-fn  put-value-path (get-fn bxb_get_buffer))]
              (when (not= src (take (count src) dest))
                [(dissoc-fn get-value-path (get-fn bxb_get_buffer))])
              (map (partial apply dissoc-fn) dissoc-const-paths-vals)
              [(dissoc-fn bxb_get_buffer (get-fn bxb_get_buffer))])

      (and map-src map-dest)
      [(map-fn dissoc-map-path
               assoc-map-path
               (interpret-template fns map-src map-dest [] []))])))

(defn create-mutations
  "Creates mutations to transmapm data. Bidirectional"
  [fns [from to] template]
  (keepcat (fn [{src from, dest to}]
             (when (and src dest) (interpret-template fns src dest [] [])))
           template))

(def mutate mf/mutate)

(def debug-mutations (partial create-mutations mf/debug-fns))
(def hmap-mutations (partial create-mutations mf/hmap-fns))
(def sql-mutations (partial create-mutations mf/sql-fns))
