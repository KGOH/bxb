(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p]]))

(defn- may-be-a-key? [x]
  (or (keyword? x)
      (integer? x)))

(defn- walk-path-forwards [get-fn assoc-fn dissoc-fn src path]
  (let [get-value (get-fn src)]
    (loop [mutations []
           [first-p & rest-p :as path] path
           cur-prefix []]
      (cond
        (or (every? may-be-a-key? path)
            (empty? path))
        (conj mutations (assoc-fn (concat cur-prefix path) get-value)
                        (dissoc-fn src))

        (may-be-a-key? first-p)
        (recur mutations
               rest-p
               (conj cur-prefix first-p))

        (or (map? first-p)
            (sequential? first-p))
        (recur (conj mutations (assoc-fn cur-prefix (constantly first-p)))
               rest-p
               cur-prefix)))))

(defn- walk-path-backwards [get-fn assoc-fn dissoc-fn src path]
  (loop [mutations [(assoc-fn src (get-fn (filter may-be-a-key? path)))]
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (every? may-be-a-key? path)
      (conj mutations (dissoc-fn (concat cur-prefix path)))

      (may-be-a-key? first-p)
      (recur mutations
             rest-p
             (conj cur-prefix first-p))

      (map? first-p)
      (recur (conj mutations (apply dissoc-fn (map conj (repeat cur-prefix) (keys first-p))))
             rest-p
             cur-prefix)

      (sequential? first-p)
      (do
        (recur (into mutations
                     (keep-indexed
                       (fn [i p] (when-not (nil? p) (dissoc-fn (conj cur-prefix i))))
                       first-p))
               rest-p
               cur-prefix))

      (empty? path)
      mutations)))

(defn- hmap-get-fn [path]
  #(get-in % (vec path)))

(defn- hmap-dissoc-fn
 ([path]
  #(dissoc-in % (vec path)))
 ([path & paths]
  #(apply dissoc-in % (vec path) (vec paths))))

(defn- hmap-assoc-fn [path get-value]
  #(assoc-in % (vec path) (get-value %)))

(defn- mutate [mutations data]
  (reduce #(%2 %1) data mutations))

(defn- sql-get-fn [path])
(defn- sql-dissoc-fn [path & paths])
(defn- sql-assoc-fn [path value])

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
    (-> (mapcat (partial apply walk get-fn assoc-fn dissoc-fn)
                (partition 2 template))
        (conj (assoc-fn [:spec_ver] (constantly to)))
        (mutate data))))

(defn transform-hmap [dir tr data]
  (transform* hmap-get-fn hmap-assoc-fn hmap-dissoc-fn dir tr data))

(defn transform-sql [dir tr data]
  (transform* sql-get-fn sql-assoc-fn sql-dissoc-fn dir tr data))
