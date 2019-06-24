(ns bxb.scratch
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [bxb.core :refer :all]
            [bxb.misc :refer :all]
            [cheshire.core :as json]))

(comment
  (json/generate-string i0)
  (def tr
    {:resourceType "VisionPrescription"
     :spec         [:stu3 :r4]
     :template     [[:reason]          [:extension [] 0 {:url "http://hl7.org/", :foo "bar"} :value :code]
                    [:dispense :prism] [:lensSpecification [:spam nil :eggs] 1 :amount]
                    [:dispense :base]  [:lensSpecification 1 :base]]})

  (println
    (str "UPDATE VisionPrescription\n"
         "SET resource = "
         (str/join (drop 3 (str/join \newline (map #(% "VisionPrescription") (sql-mutations [:r4 :stu3] tr)))))
         ";"))

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

