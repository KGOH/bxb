(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]))

(defn- walk-path-forwards [value path]
  (loop [[first-p & rest-p :as path] path
         cur-prefix []
         traveled-paths []]
    (cond
      (or (not first-p)
          (every? keyword? path))
      (conj
        traveled-paths
        [(concat cur-prefix path) value])

      (map? first-p)
      (recur
        rest-p
        cur-prefix
        (concat traveled-paths
                (map (fn [[k v]] [(conj cur-prefix k) v])
                     first-p)))

      (keyword? first-p)
      (recur
        rest-p
        (conj cur-prefix first-p)
        traveled-paths))))

(defn- dissoc-paths [data [p & rest-p]]
  (if p
    (recur (dissoc-in data p) rest-p)
    data))

(defn- assoc-paths [data [[p v :as first-p] & rest-p]]
  (if first-p
    (recur (assoc-in data p v) rest-p)
    data))

(defn- construct-paths-to-dissoc [template]
  (take-nth 2 template))

(defn- construct-paths-to-assoc [template data]
  (->> (map (fn [[src path]] (walk-path-forwards (get-in data src) path))
            (partition 2 template))
       (apply concat)))

(defn- transform-forwards [{:keys [template]} data]
  (-> data
      (dissoc-paths (construct-paths-to-dissoc template))
      (assoc-paths  (construct-paths-to-assoc  template data))))

(defn- transform-backwards [tr data]
  :not-implemented)

(defn transform
  "Transformes data bidirectionally"
  [[from to                                                       :as dir]
   {tr-type :resourceType, [tr-from tr-to :as tr-spec] :spec,     :as tr}
   {d-type  :resourceType, d-spec-v                    :spec_ver, :as data}]
  {:pre  [(and (= (set dir) (set tr-spec))
               (= from      d-spec-v)
               (= tr-type   d-type))] ; maybe excess
   :post [(= (:spec_ver %) to)]}
  (let [trans-func
        (if (= dir tr-spec)
            transform-forwards
            transform-backwards)]
    (assoc (trans-func tr data) :spec_ver to)))
