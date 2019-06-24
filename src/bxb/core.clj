(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p]]
            [bxb.mutate-fns :refer :all]))

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

(defn create-mutations*
  "Creates mutations to transform data. Bidirectional"
  [get-fn assoc-fn dissoc-fn [from to :as dir] {:keys [spec template] :as tr}]
  {:pre  [(and (= (set dir) (set spec)))]}
  (let [walk (if (= dir spec)
                 walk-path-forwards
                 walk-path-backwards)]
    (-> (mapcat (partial apply walk get-fn assoc-fn dissoc-fn)
                (partition 2 template)))))

(defn mutate [mutations data]
  (reduce #(%2 %1) data mutations))

(defn hmap-mutations [dir tr]
  (create-mutations* hmap-get-fn hmap-assoc-fn hmap-dissoc-fn dir tr))

(defn sql-mutations [dir tr]
  (create-mutations* sql-get-fn sql-assoc-fn sql-dissoc-fn dir tr))
