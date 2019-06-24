(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]))

(defn may-be-a-key? [x]
  (or (keyword? x)
      (integer? x)))



(defn- walk-path-forwards [data src path]
  (let [value (get-in data src)]
    (loop [data (dissoc-in data src)
           [first-p & rest-p :as path] path
           cur-prefix []]
      (cond
        (every? may-be-a-key? path)
        (assoc-in data (concat cur-prefix path) value)

        (may-be-a-key? first-p)
        (recur data
               rest-p
               (conj cur-prefix first-p))

        (or (map? first-p)
            (sequential? first-p))
        (recur (assoc-in data cur-prefix first-p)
               rest-p
               cur-prefix)

        (empty? path)
        data))))

(defn- walk-path-backwards [data src path]
  (loop [data (assoc-in data src (get-in data (filter may-be-a-key? path)))
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? may-be-a-key? path)
      (dissoc-in data (concat cur-prefix path))

      (may-be-a-key? first-p)
      (recur data
             rest-p
             (conj cur-prefix first-p))

      (map? first-p)
      (recur (apply update-in data cur-prefix dissoc (keys first-p))
             rest-p
             cur-prefix)

      (sequential? first-p)
      (do
        (recur (transduce
                 (keep-indexed (fn [i p] (when-not (nil? p) (conj cur-prefix i))))
                 (completing dissoc-in)
                 data
                 first-p)
               rest-p
               cur-prefix))

      (empty? path)
      data)))

(defn transform
  "Transformes data bidirectionally"
  [[from to :as dir]
   {tr-spec :spec, template :template, :as tr}
   {d-spec-v :spec_ver, :as data}]
  {:pre  [(and (= (set dir) (set tr-spec))
               (= from d-spec-v))]}
  (let [walk (if (= dir tr-spec)
                 walk-path-forwards
                 walk-path-backwards)]
    (-> (reduce (partial apply walk)
                data
                (partition 2 template))
        (assoc :spec_ver to))))
