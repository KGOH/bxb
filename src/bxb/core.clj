(ns bxb.core
  (:require [bxb.misc         :as misc]
            [bxb.mutate.hmap  :as hmap]
            [bxb.mutate.sql   :as sql]
            [bxb.mutate.debug :as debug]
            [utiliva.core     :refer [keepcat]]))

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

(defn- interpret-template [{:keys [const-fn search-fn map-fn get-fn assoc-fn dissoc-fn] :as fns} src dest]
  (let [{get-value-path :walked-path, dissoc-const-paths-vals :const-paths-vals, {map-src :path, dissoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn src)

        {put-value-path :walked-path, assoc-const-paths-vals  :const-paths-vals, {map-dest :path, assoc-map-path :walked-path} :map}
        (walk-path const-fn search-fn dest)

        bxb_get_buffer
        [(const-fn :bxb_get_buffer)]]  ;; Maybe need another way to avoid dissocing the same key where new data was assoced?
    (cond
      (and get-value-path put-value-path)
      (concat [(assoc-fn bxb_get_buffer (get-fn get-value-path))]
              (map (partial apply assoc-fn) assoc-const-paths-vals)
              [(assoc-fn  put-value-path (get-fn bxb_get_buffer))]
              (when (not= (vec (take (count src) dest)) (vec src))
                [(dissoc-fn get-value-path (get-fn bxb_get_buffer))])
              (map (partial apply dissoc-fn) dissoc-const-paths-vals)
              [(dissoc-fn bxb_get_buffer (get-fn bxb_get_buffer))])

      (and map-src map-dest)
      [(map-fn dissoc-map-path
               assoc-map-path
               (interpret-template fns map-src map-dest))])))

(defn create-mutations
  "Creates mutations to transmapm data. Bidirectional"
  [fns [from to] template]
  (keepcat (fn [{src from, dest to}]
             (when (and src dest) (interpret-template fns src dest)))
           template))

(def debug-mutations (partial create-mutations debug/fns))
(def hmap-mutations  (partial create-mutations hmap/fns))
(def sql-mutations   (partial create-mutations sql/fns))

(def mutate misc/mutate)
