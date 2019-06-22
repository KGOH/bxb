(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]))

(defn- walk-path-forwards [data value path]
  (loop [data data
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (or (not first-p) (every? keyword? path))
      (assoc-in data (concat cur-prefix path) value)

      (map? first-p)
      (recur
        (assoc-in data cur-prefix first-p)
        rest-p
        cur-prefix)

      (keyword? first-p)
      (recur
        data
        rest-p
        (conj cur-prefix first-p)))))

(defn- dissoc-paths [data [first-p & rest-p]]
  (if first-p
    (recur (dissoc-in data first-p)
           rest-p)
    data))

(defn- assoc-paths [data [[src path :as first-t] & rest-t :as template]]
  (if first-t
    (recur (walk-path-forwards data (get-in data src) path)
           rest-t)
    data))

(defn- transform-forwards [{:keys [template]} data]
  (-> data
      (assoc-paths (partition 2 template))
      (dissoc-paths (take-nth 2 template))))

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
