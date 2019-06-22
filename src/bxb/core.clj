(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]))

(defn- walk-path-forwards [data value path]
  (loop [data data
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (or (empty? path) (every? keyword? path))
      (assoc-in data (concat cur-prefix path) value)

      (keyword? first-p)
      (recur
        data
        rest-p
        (conj cur-prefix first-p))

      (map? first-p)
      (recur
        (assoc-in data cur-prefix first-p)
        rest-p
        cur-prefix))))


(defn- transform-forwards [[src path & rest-t] data]
  (if src
    (recur rest-t (walk-path-forwards (dissoc-in data src) (get-in data src) path))
    data))

(defn- transform-backwards [tr data]
  :not-implemented)

(defn transform
  "Transformes data bidirectionally"
  [[from to :as dir]
   {tr-type :resourceType, [tr-from tr-to :as tr-spec] :spec, template :template, :as tr}
   {d-type :resourceType, d-spec-v :spec_ver, :as data}]
  {:pre  [(and (= (set dir) (set tr-spec))
               (= from      d-spec-v)
               (= tr-type   d-type))] ; maybe excess
   :post [(= (:spec_ver %) to)]}
  (let [trans-func
        (if (= dir tr-spec)
            transform-forwards
            transform-backwards)]
    (assoc (trans-func template data) :spec_ver to)))
