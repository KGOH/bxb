(ns bxb.core-test
  (:require [clojure.test :refer :all]
            [bxb.core :refer :all]))

(deftest test-stu3-r4
  (def prescr-stu3
    {:resourceType "VisionPrescription"
     :reason "ups"
     :dispense {:prism 5
                :base 1}})

  (def prescr-r4
    {:resourceType "VisionPrescription"
     :extension [{:url "http://hl7.org/reason"
                  :foo "bar"
                  :value {:code "ups"}}]
     :lensSpecification [:spam
                         {:amount 5
                          :base 1}
                         :eggs]})

  (def tr
    {:resourceType "VisionPrescription"
     :spec         [:stu3 :r4]
     :template     [[:reason]          [:extension [] 0 {:url "http://hl7.org/reason", :foo "bar"} :value :code]
                    [:dispense :prism] [:lensSpecification [:spam nil :eggs] 1 :amount]
                    [:dispense :base]  [:lensSpecification 1 :base]]})

  (def tr-sql
    (str "UPDATE VisionPrescription "
         "SET resource = "))

  (testing "hashmap mutation forwards stu3->r4"
    (is (= prescr-r4
           (mutate (hmap-mutations [:stu3 :r4] tr)
                   prescr-stu3))))

  (testing "hashmap mutation backwards stu3<-r4"
    (is (= prescr-stu3
           (mutate (hmap-mutations [:r4 :stu3] tr)
                   prescr-r4)))))

(comment
  (run-tests)
  (comment))

