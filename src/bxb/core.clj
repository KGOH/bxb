(ns bxb.core)

(def prescr-r3
  {:resourceType "VisionPrescription"
   :spec_ver :r3
   :reason "ups"
   :dispense {:prism 5
              :base 1}})

(def prescr-r4
  {:resourceType "VisionPrescription"
   :spec_ver :r4
   :extension {:url "http://hl7.org/fhir/3.0/StructureDefinition/extension-VisionPrescription.Reason"
               :value {:code "ups"}}
   :lensSpecification {:amount 5
                       :base 1}})

(def tr
  {:resourceType "VisionPrescription"
   :spec         [:r3 :r4]
   :transform    [[:reason]          [:extension {:url "reason" :foo "ups"} :value :code]
                  [:dispense :prism] [:lensSpecification :amount]]})

(defn transform [[from to] tr data]
  {:pre [(and (= from (:spec_ver tr))
              (= to   (:spec_ver data)))]}
  data)


(= (transform [:r3 :r4] tr prescr-r3)
   prescr-r4)

(defn to-sql [tr dir])

(comment
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

