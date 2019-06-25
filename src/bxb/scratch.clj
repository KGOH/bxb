(ns bxb.scratch
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [bxb.core :refer :all]
            [bxb.misc :refer :all]
            [cheshire.core :as json]))

(comment
  (json/generate-string i0)
  (def tr
    [{:stu3 [:reason]
      :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}
     {:stu3 [:dispense :prism]
      :r4   [:lensSpecification :amount]}
     {:stu3 [:dispense :base]
      :r4   [:lensSpecification :base]}])
  (println (str/join (drop 3 (str/join \newline (map #(% "resource") (sql-mutations [:r4 :stu3] tr))))))
  (println (str/join (drop 3 (str/join \newline (map #(% "resource") (sql-mutations [:stu3 :r4] tr))))))

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

