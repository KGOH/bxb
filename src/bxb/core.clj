(ns bxb.core
  (:require [bxb.misc :refer [dissoc-in]]
            [clojure.pprint :refer [pprint]]))

(def prescr-r3
  {:resourceType "VisionPrescription"
   :spec_ver :r3
   :reason "ups"
   :dispense {:prism 5
              :base 1}})

(def prescr-r4
  {:resourceType "VisionPrescription"
   :spec_ver :r4
   :extension {:url "http://hl7.org/"
               :value {:code "ups"}}
   :lensSpecification {:amount 5
                       :base 1}})

(def tr
  {:resourceType "VisionPrescription"
   :spec         [:r3 :r4]
   :template     [[:reason]          [:extension {:url "http://hl7.org/"} :value :code]
                  [:dispense :prism] [:lensSpecification :amount]
                  [:dispense :base]  [:lensSpecification :base]]})

(defn- walk-path-forwards [data path]
  (loop [[first-p & rest-p :as path] path
         cur-prefix []
         traveled-paths []]
    (cond
      (or (not first-p)
          (every? keyword? path))
      (conj
        traveled-paths
        [(concat cur-prefix path) data])

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
  (as-> data $
    (if (= dir tr-spec)
        (transform-forwards  tr $)
        (transform-backwards tr $))
    (assoc $ :spec_ver to)))

(= (transform [:r3 :r4] tr prescr-r3)
   prescr-r4)

(defn to-sql [tr dir])

(comment
  "
  update visionpre
  set resource = (resource - '{reason, dispense})'
                  [:dispense :base]  [:lensSpecification :base]]})
                  [:dispense :base]  [:lensSpecification :base]]})
  || jsonb_build_object('extensions', resource->'extension' + jobj('value'...))
  "
  [[:a] [:b]]
  [[:a :b] [:c]]
  [[:a] [:b :c]])
;;https://www.postgresql.org/docs/11/functions-json.html
;; ||
;; -
;; jsonb_build_object

