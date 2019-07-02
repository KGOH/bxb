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
                 :reason       "ups"}
      :r4       {:resourceType      "VisionPrescription"
                 :extension         {:url "http://hl7.org/reason"
                                     :value {:code "ups"}}}
      :template [{:stu3 [:reason]
                  :r4   [:extension {:url "http://hl7.org/reason"} :value :code]}]}

     {:desc "hashmap mutation forwards %s->%s with creating extension vec"
      :spec [:stu3 :r4]
      :stu3 {:resourceType "VisionPrescription"
             :reason       "ups"}
      :r4   {:resourceType      "VisionPrescription"
             :extension         [{:url   "http://hl7.org/reason"
                                  :value {:code "ups"}}]}
      :template [{:stu3 [:reason]
                  :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}]}

     {:desc "hashmap mutation forwards %s->%s with existing extension vec"
      :spec [:stu3 :r4]
      :stu3 {:resourceType "VisionPrescription"
             :reason "ups"
             :extension [{:url "http://hl7.org/fact"
                          :value {:code "fucct"}}]}
      :r4   {:resourceType "VisionPrescription"
             :extension [{:url "http://hl7.org/fact"
                          :value {:code "fucct"}}
                         {:url "http://hl7.org/reason"
                          :value {:code "ups"}}]}
      :template [{:stu3 [:reason]
                  :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}]}

     {:desc     "hashmap mutation %s->%s remap with missing source"
      :spec     [:stu3 :r4]
      :stu3     {:resourceType "VisionPrescription"}
      :r4       {:resourceType "VisionPrescription"}
      :template [{:stu3 [:dispense :prism]
                  :r4   [:lensSpecification :amount]}]}
     {:desc     "hashmap mutation %s->%s mapped name remap"
      :spec     [:v1 :v2]
      :v1       {:i {:a [{:b 1} {:b 2}]}}
      :v2       {:i {:x [{:y 1} {:y 2}]}}
      :template [{:v1 [:i [:a] :b]
                  :v2 [:i [:x] :y]}]}
     {:desc     "hashmap mutation %s->%s nested map name remap"
      :spec     [:v1 :v2]
      :v1       {:a [{:b [{:c 1} {:c 2}]}
                     {:b [{:c 3} {:c 4}]}]}
      :v2       {:x [{:y [{:z 1} {:z 2}]}
                     {:y [{:z 3} {:z 4}]}]}
      :template [{:v1 [[:a] [:b] :c]
                  :v2 [[:x] [:y] :z]}]}
     {:desc     "hashmap mutation %s->%s add const into root"
      :spec     [:v1 :v2]
      :v1       {:a 1}
      :v2       {:x 1
                 :required "field"}
      :template [{:v1 [:a]
                  :v2 [{:required "field"} :x]}]}
     {:desc     "hashmap mutation %s->%s add const into root without source"
      :spec     [:v1 :v2]
      :v1       {:a 1}
      :v2       {:a 1
                 :required "field"}
      :template [{:v1 []
                  :v2 [{:required "field"}]}]}])

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
  (comment))

