(ns bxb.core-test
  (:require [clojure.test :refer :all]
            [bxb.core :refer :all]))

(deftest test-stu3-r4
  (def prescr-stu3
    {:resourceType "VisionPrescription"
     :spec_ver :stu3
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
     :spec         [:stu3 :r4]
     :template     [[:reason]          [:extension {:url "http://hl7.org/", :foo "bar"} :value :code]
                    [:dispense :prism] [:lensSpecification :amount]
                    [:dispense :base]  [:lensSpecification :base]]})

  (testing "transformation forwards stu3->r4"
    (is (= (transform [:stu3 :r4] tr prescr-stu3)
           prescr-r4)))

  (testing "transformation backwards stu3<-r4"
    (is (= (transform [:r4 :stu3] tr prescr-r4)
           prescr-stu3))))

(run-tests)
