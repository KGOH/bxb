(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p]]
            [bxb.mutate-fns :refer :all]))

(defn- may-be-a-key? [x]
  (or (keyword? x)
      (integer? x)
      (string?  x)))

(defn- walk-path [get-fn assoc-fn dissoc-fn src path]
  (loop [mutations []
         [first-p & rest-p :as path] path
         cur-prefix []]
    (cond
      (or (every? may-be-a-key? path)
          (empty? path))
      (conj mutations (assoc-fn (concat cur-prefix path) (get-fn src))
                      (dissoc-fn src))

      (may-be-a-key? first-p)
      (recur mutations
             rest-p
             (conj cur-prefix first-p))

      (or (map? first-p)
          (sequential? first-p))
      (recur (conj mutations (assoc-fn cur-prefix (constantly first-p)))
             rest-p
             cur-prefix))))

(defn- walk-path-backwards [get-fn assoc-fn dissoc-fn path src]
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

(defn create-mutations*
  "Creates mutations to transform data. Bidirectional"
  [get-fn assoc-fn dissoc-fn [from to] template]
  (-> (mapcat (fn [{src from, dest to}] (walk-path get-fn assoc-fn dissoc-fn src dest))
              template)))

(defn mutate [mutations data]
  (reduce #(%2 %1) data mutations))

(defn hmap-mutations [dir template]
  (create-mutations* hmap-get-fn hmap-assoc-fn hmap-dissoc-fn dir template))

(defn sql-mutations [dir template]
  (create-mutations* sql-get-fn sql-assoc-fn sql-dissoc-fn dir template))
