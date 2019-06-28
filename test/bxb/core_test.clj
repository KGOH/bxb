(ns bxb.core-test
  (:require [clojure.test :refer :all]
            [bxb.core :refer :all]))

(deftest test-stu3-r4
  (def test-cases
    [{:desc     "hashmap mutation %s->%s nested name remap"
      :spec     [:stu3 :r4]
      :stu3     {:resourceType "VisionPrescription"
                 :reason       "ups"
                 :dispense     {:prism 5
                                :base  1}}
      :r4       {:resourceType      "VisionPrescription"
                 :code              "ups"
                 :lensSpecification {:amount 5
                                     :base 1}}
      :template [{:stu3 [:reason]
                  :r4   [:code]}
                 {:stu3 [:dispense :prism]
                  :r4   [:lensSpecification :amount]}
                 {:stu3 [:dispense :base]
                  :r4   [:lensSpecification :base]}]}

     {:desc     "hashmap mutation %s->%s (un)nesting name remap"
      :spec     [:stu3 :r4]
      :stu3     {:resourceType "VisionPrescription"
                 :reason       "ups"
                 :dispense     {:prism 5
                                :base  1}}
      :r4       {:resourceType "VisionPrescription"
                 :code         "ups"
                 :amount       5
                 :base         1}
      :template [{:stu3 [:reason]
                  :r4   [:code]}
                 {:stu3 [:dispense :prism]
                  :r4   [:amount]}
                 {:stu3 [:dispense :base]
                  :r4   [:base]}]}

     {:desc     "hashmap mutation %s->%s with adding const by the path"
      :spec     [:stu3 :r4]
      :stu3     {:resourceType "VisionPrescription"
                 :reason       "ups"
                 :dispense     {:prism 5
                                :base 1}}
      :r4       {:resourceType      "VisionPrescription"
                 :extension         {:url "http://hl7.org/reason"
                                     :value {:code "ups"}}
                 :lensSpecification {:amount 5
                                     :base 1}}
      :template [{:stu3 [:reason]
                  :r4   [:extension {:url "http://hl7.org/reason"} :value :code]}
                 {:stu3 [:dispense :prism]
                  :r4   [:lensSpecification :amount]}
                 {:stu3 [:dispense :base]
                  :r4   [:lensSpecification :base]}]}

     {:desc "hashmap mutation forwards %s->%s with creating extension vec"
      :spec [:stu3 :r4]
      :stu3 {:resourceType "VisionPrescription"
             :reason       "ups"
             :dispense     {:prism 5
                            :base  1}}
      :r4   {:resourceType      "VisionPrescription"
             :extension         [{:url   "http://hl7.org/reason"
                                  :value {:code "ups"}}]
             :lensSpecification {:amount 5
                                 :base   1}}
      :template [{:stu3 [:reason]
                  :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}
                 {:stu3 [:dispense :prism]
                  :r4   [:lensSpecification :amount]}
                 {:stu3 [:dispense :base]
                  :r4   [:lensSpecification :base]}]}

     {:desc "hashmap mutation forwards %s->%s with existing extension vec"
      :spec [:stu3 :r4]
      :stu3 {:resourceType "VisionPrescription"
             :reason "ups"
             :extension [{:url "http://hl7.org/fact"
                          :value {:code "fucct"}}]
             :dispense {:prism 5
                        :base 1}}
      :r4   {:resourceType "VisionPrescription"
             :extension [{:url "http://hl7.org/fact"
                          :value {:code "fucct"}}
                         {:url "http://hl7.org/reason"
                          :value {:code "ups"}}]
             :lensSpecification {:amount 5
                                 :base 1}}
      :template [{:stu3 [:reason]
                  :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}
                 {:stu3 [:dispense :prism]
                  :r4   [:lensSpecification :amount]}
                 {:stu3 [:dispense :base]
                  :r4   [:lensSpecification :base]}]}

     {:desc     "hashmap mutation %s->%s remap with missing source"
      :spec     [:stu3 :r4]
      :stu3     {:resourceType "VisionPrescription"
                 :reason       "ups"}
      :r4       {:resourceType      "VisionPrescription"
                 :code              "ups"}
      :template [{:stu3 [:reason]
                  :r4   [:code]}
                 {:stu3 [:dispense :prism]
                  :r4   [:lensSpecification :amount]}
                 {:stu3 [:dispense :base]
                  :r4   [:lensSpecification :base]}]}])

  (mapv (fn [{:keys [desc spec template] :as t}]
          (mapv (fn [[from to]]
                  (testing (format desc from to)
                    (is (= (to t)
                           (mutate (hmap-mutations [from to] template)
                                   (from t))))))
                [spec (reverse spec)]))
        test-cases))

(comment
  (run-tests)
; {:a [{:a1  :b2} {:a1 :b3}]}
; {:a [{:b2} {:a1}]}

; []  ;required fileds, константы без источника в корне
; [:extension [:url] :value :code] ;; map

; [:extension [{:url "http://hl7.org/reason"}] :value :code]
; [:extension [{:a1 :b3}] :value :code] ;;

  (comment))

