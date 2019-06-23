(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]))

(defn- walk-path-forwards [data value path]
  (loop [data data
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? #(or (keyword? %)  ;maybe move out of the loop?
                   (integer? %))
              path)
      (assoc-in data (concat cur-prefix path) value)

      (or (keyword? first-p)
          (integer? first-p))
      (recur data
             rest-p
             (conj cur-prefix first-p))

      (or (map? first-p)
          (sequential? first-p))
      (recur (assoc-in data cur-prefix first-p)
             rest-p
             cur-prefix)

      (empty? path)
      data)))

(defn- transform-forwards [[src path & rest-t] data]
  (if-not src
    data
    (recur rest-t (walk-path-forwards (dissoc-in data src)
                                      (get-in data src)
                                      path))))

(defn- transform-backwards [[src path & rest-t] data]
  :not-implemented)

(defn transform
  "Transformes data bidirectionally"
  [[from to :as dir]
   {tr-spec :spec, template :template, :as tr}
   {d-spec-v :spec_ver, :as data}]
  {:pre  [(and (= (set dir) (set tr-spec))
               (= from d-spec-v))]
   :post [(= (:spec_ver %) to)]}
  (let [transform* (if (= dir tr-spec)
                       transform-forwards
                       transform-backwards)]
    (assoc (transform* template data) :spec_ver to)))
