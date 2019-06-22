(ns bxb.core-test
  (:require [clojure.test :refer :all]
            [bxb.core :refer :all]))

(deftest test-r3-r4
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
                 :foo "bar"
                 :value {:code "ups"}}
     :lensSpecification {:amount 5
                         :base 1}})

  (def tr
    {:resourceType "VisionPrescription"
     :spec         [:r3 :r4]
     :template     [[:reason]          [:extension {:url "http://hl7.org/", :foo "bar"} :value :code]
                    [:dispense :prism] [:lensSpecification :amount]
                    [:dispense :base]  [:lensSpecification :base]]})

  (testing "transformation forwards r3->r4"
    (is (= (transform [:r3 :r4] tr prescr-r3)
           prescr-r4)))

  (testing "transformation backwards r3<-r4"
    (is (= (transform [:r4 :r3] tr prescr-r4)
           prescr-r3))))

(run-tests)
