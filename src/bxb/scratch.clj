(ns bxb.scratch
  (:require [clojure.pprint :refer [pprint]]
            [bxb.core :refer :all]
            [bxb.misc :refer :all]))

(comment
  (def tr
    {:resourceType "VisionPrescription"
     :spec         [:stu3 :r4]
     :template     [[:reason]          [:extension [] 0 {:url "http://hl7.org/", :foo "bar"} :value :code]
                    [:dispense :prism] [:lensSpecification [:spam nil :eggs] 1 :amount]
                    [:dispense :base]  [:lensSpecification 1 :base]]})

  (p (map #(% "VisionPrescription") (sql-mutations [:stu3 :r4] tr)))
  (p (map #(% "VisionPrescription") (sql-mutations [:r4 :stu3] tr))))

(comment
  (defn to-sql [tr dir])
  "
  update visionpre
  set resource = (resource - '{reason, dispense})'
  || jsonb_build_object('extensions', resource->'extension' + jobj('value'...))
  "
  [[:a] [:b]]
  [[:a :b] [:c]]
  [[:a] [:b :c]])
;;https://www.postgresql.org/docs/11/functions-json.html
;; ||
;; -
;; jsonb_build_object

