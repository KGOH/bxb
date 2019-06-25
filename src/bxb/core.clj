(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in p may-be-a-key? single-elem?]]
            [bxb.mutate-fns :refer :all]))

(defn- walk-put-in [assoc-fn dest get-value]
  (println "walk-put-in\t" dest)
  (loop [mutations []
         [first-d & rest-d :as dest] dest
         cur-prefix []]
    (println "looking at\t" first-d)
    (cond
      (every? may-be-a-key? dest)
      (do
        (println "it and every next are keys")
        (conj mutations
              (assoc-fn (concat cur-prefix dest) get-value)))

      (may-be-a-key? first-d)
      (do
        (println "it is a key")
        (recur mutations
               rest-d
               (conj cur-prefix first-d)))

      (map? first-d)
      (do
        (println "it is a map")
        (recur (conj mutations (assoc-fn cur-prefix (constantly first-d)))
               rest-d
               cur-prefix))

      (and (sequential? first-d)
           (single-elem? first-d)
           (map? (first first-d)))
      (do
        (println "I need to find a hmap in a seq" first-d) ; TODO: add upsert
        (recur (conj mutations (assoc-fn cur-prefix (constantly first-d)))
               rest-d
               (conj cur-prefix 0))))))


(defn- walk-pop-in [get-fn dissoc-fn src]
  (println "walk-pop-in\t" src)
  (loop [mutations []
         [first-s & rest-s :as src] src
         cur-prefix []]
    (println "looking at\t" first-s)
    (cond
      (every? may-be-a-key? src)
      (let [p (concat cur-prefix src)
            get-value (get-fn p)
            pop-value (dissoc-fn p)]
        (println "it and every next are keys")
        {:get-value get-value
         :dissoc-mutations (conj mutations pop-value)})

      (may-be-a-key? first-s)
      (do
        (println "it is a key")
        (recur mutations
               rest-s
               (conj cur-prefix first-s)))

      (map? first-s)
      (do
        (println "it is a map")
        (recur (into mutations (map (fn [k] (dissoc-fn (conj cur-prefix k))) (keys first-s)))
               rest-s
               cur-prefix))

      (and (sequential? first-s)
           (single-elem? first-s)
           (map? (first first-s)))
      (do
        (println "I need to find a hmap in a seq" first-s) ; TODO add search
        (recur (into mutations (map (fn [k] (dissoc-fn (conj cur-prefix 0 k))) (keys first-s)))
               rest-s
               cur-prefix)))))

(defn- walk-path [get-fn assoc-fn dissoc-fn src dest]
  (println "walk-path")
  (let [{:keys [get-value dissoc-mutations]} (walk-pop-in get-fn dissoc-fn src)
        assoc-mutations (walk-put-in assoc-fn dest get-value)]
    (into assoc-mutations dissoc-mutations)))

     ; (sequential? first-p)
     ; (do
     ;   (recur (into mutations
     ;                (keep-indexed
     ;                  (fn [i p] (when-not (nil? p) (dissoc-fn (conj cur-prefix i))))
     ;                  first-p))
     ;          rest-p
     ;          cur-prefix))


(defn create-mutations*
  "Creates mutations to transform data. Bidirectional"
  [get-fn assoc-fn dissoc-fn [from to] template]
  (println "creating mutations" from "->" to)
  (-> (mapcat (fn [{src from, dest to}] (walk-path get-fn assoc-fn dissoc-fn src dest))
              template)))

(defn mutate [mutations data]
  (println "mutate")
  (reduce (fn [d m] (println "applying" m "to" d)
                    (m d))
          data
          mutations))

(defn hmap-mutations [dir template]
  (create-mutations* hmap-get-fn hmap-assoc-fn hmap-dissoc-fn dir template))

(defn sql-mutations [dir template]
  (create-mutations* sql-get-fn sql-assoc-fn sql-dissoc-fn dir template))
