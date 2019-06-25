(ns bxb.core-test
  (:require [clojure.test :refer :all]
            [bxb.core :refer :all]))

(deftest test-stu3-r4
  (def VisionPrescription-stu3
    {:resourceType "VisionPrescription"
     :reason "ups"
     :extension [{:url "http://hl7.org/fact"
                  :value {:code "fucct"}}]
     :dispense {:prism 5
                :base 1}})

  (def VisionPrescription-r4
    {:resourceType "VisionPrescription"
     :extension [{:url "http://hl7.org/fact"
                  :value {:code "fucct"}}
                 {:url "http://hl7.org/reason"
                  :value {:code "ups"}}]
     :lensSpecification {:amount 5
                          :base 1}})

  (def VisionPrescription-template
    [{:stu3 [:reason]
      :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}
     {:stu3 [:dispense :prism]
      :r4   [:lensSpecification :amount]}
     {:stu3 [:dispense :base]
      :r4   [:lensSpecification :base]}])

  (testing "hashmap mutation forwards stu3->r4"
    (is (= VisionPrescription-r4
           (mutate (hmap-mutations [:stu3 :r4] VisionPrescription-template)
                   VisionPrescription-stu3))))

  (testing "hashmap mutation backwards stu3<-r4"
    (is (= VisionPrescription-stu3
           (mutate (hmap-mutations [:r4 :stu3] VisionPrescription-template)
                   VisionPrescription-r4)))))

(comment
  (run-tests)
  (comment))

