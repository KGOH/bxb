(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]))

(defn- may-be-a-key? [x]
  (or (keyword? x)
      (integer? x)))

(defn- walk-path-forwards [get-fn assoc-fn dissoc-fn data src path]
  (let [value (get-fn data src)]
    (loop [data (dissoc-in data src)
           [first-p & rest-p :as path] path
           cur-prefix []]
      (cond
        (every? may-be-a-key? path)
        (assoc-fn data (concat cur-prefix path) value)

        (may-be-a-key? first-p)
        (recur data
               rest-p
               (conj cur-prefix first-p))

        (or (map? first-p)
            (sequential? first-p))
        (recur (assoc-fn data cur-prefix first-p)
               rest-p
               cur-prefix)

        (empty? path)
        data))))

(defn- walk-path-backwards [get-fn assoc-fn dissoc-fn data src path]
  (loop [data (assoc-fn data src (get-fn data (filter may-be-a-key? path)))
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? may-be-a-key? path)
      (dissoc-fn data (concat cur-prefix path))

      (may-be-a-key? first-p)
      (recur data
             rest-p
             (conj cur-prefix first-p))

      (map? first-p)
      (recur (apply dissoc-fn data (map conj (repeat cur-prefix) (keys first-p)))
             rest-p
             cur-prefix)

      (sequential? first-p)
      (do
        (recur (transduce
                 (keep-indexed (fn [i p] (when-not (nil? p) (conj cur-prefix i))))
                 (completing dissoc-fn)
                 data
                 first-p)
               rest-p
               cur-prefix))

      (empty? path)
      data)))

(defn- sql-get-fn [data path])
(defn- sql-dissoc-fn [data path])
(defn- sql-assoc-fn [data path value])

(defn transform*
  "Transformes data bidirectionally"
  [get-fn
   assoc-fn
   dissoc-fn
   [from to :as dir]
   {tr-spec :spec, template :template, :as tr}
   {d-spec-v :spec_ver, :as data}]
  {:pre  [(and (= (set dir) (set tr-spec))
               (= from d-spec-v))]}
  (let [walk (if (= dir tr-spec)
                 walk-path-forwards
                 walk-path-backwards)]
    (-> (reduce (partial apply walk get-fn assoc-fn dissoc-fn)
                data
                (partition 2 template))
        (assoc-fn [:spec_ver] to))))

(defn transform-hmap [dir tr data]
  (transform* get-in assoc-in dissoc-in dir tr data))

(defn transform-sql [dir tr data]
  (transform* sql-get-fn sql-assoc-fn sql-dissoc-fn dir tr data))
