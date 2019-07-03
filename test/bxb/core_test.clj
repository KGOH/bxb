(ns bxb.core-test
  (:require [clojure.test :refer :all]
            [bxb.core :refer :all]
            [bxb.misc :refer [load-edn]]))

(deftest test-hmap-mutations
  (let [test-cases
        [{:desc     "hashmap mutation %s->%s nested name remap"
          :spec     [:stu3 :r4]
          :stu3     {:resourceType "VisionPrescription"
                     :reason       "ups"
                     :dispense     {:prism 5
                                    :base  1}}
          :r4       {:resourceType      "VisionPrescription"
                     :code              "ups"
                     :dispense {:amount 5
                                :base 1}}
          :template [{:stu3 [:reason]
                      :r4   [:code]}
                     {:stu3 [:dispense :prism]
                      :r4   [:dispense :amount]}
                     {:stu3 [:dispense :base]
                      :r4   [:dispense :base]}]}

         {:desc     "hashmap mutation %s->%s with array destruction"
          :spec     [:stu3 :r4]
          :stu3     {:resourceType "VisionPrescription"
                     :dispense     [{:prism 5}]}
          :r4       {:resourceType "VisionPrescription"
                     :amount       5}
          :template [{:stu3 [:dispense 0 :prism]
                      :r4   [:amount]}]}

         {:desc     "hashmap mutation %s->%s (un)nesting name remap"
          :spec     [:stu3 :r4]
          :stu3     {:resourceType "VisionPrescription"
                     :reason       "ups"
                     :dispense     {:prism 5
                                    :base  1}}
          :r4       {:resourceType "VisionPrescription"
                     :reason         {:code "ups"}
                     :amount       5
                     :base         1}
          :template [{:stu3 [:reason]
                      :r4   [:reason :code]}
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
          :spec     [:stu3 :r4]                             ;; TODO: дефолт значения
          :stu3     {:resourceType "VisionPrescription"}    ;; :^default{:value 0}
          :r4       {:resourceType "VisionPrescription"}
          :template [{:stu3 [:dispense :prism]
                      :r4   [:lensSpecification :amount]}]}

         {:desc     "hashmap mutation %s->%s mapped name remap"
          :spec     [:v1 :v2]
          :v1       {:i {:a [{:b 1, :c -1} {:b 2, :c -2}]}}
          :v2       {:i {:a [{:y 1, :c -1} {:y 2, :c -2}]}}
          :template [{:v1 [:i [:a] :b]
                      :v2 [:i [:a] :y]}]}

         {:desc     "hashmap mutation %s->%s mapped name remap with missing source"
          :spec     [:v1 :v2]
          :v1       {:i {:not-a 0}}
          :v2       {:i {:not-a 0}}
          :template [{:v1 [:i [:a] :b]
                      :v2 [:i [:a] :y]}]}

         {:desc     "hashmap mutation %s->%s root array mapped name remap"
          :spec     [:v1 :v2]
          :v1       {:a [{:b 1, :c -1} {:b 2, :c -2}]}
          :v2       {:x [{:y 1, :c -1} {:y 2, :c -2}]}
          :template [{:v1 [[:a] :b]
                      :v2 [[:x] :y]}]}

        ;{:desc     "hashmap mutation %s->%s filtered mapped name remap"
        ; :spec     [:v1 :v2]
        ; :v1       {:i {:a [{:b 1, :flag true} {:b 2, :flag false}]}}
        ; :v2       {:i {:x [{:y 1, :flag true} {:b 2, :flag false}]}}
        ; :template [{:v1 [:i [:a] [{:flag true}] :b]     ;;TODO: продумать синтаксис матчинга
        ;             :v2 [:i [:x] [{:flag true}] :y]}]}  ;; может быть, мета-ключем

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
                     :r "field"}
          :template [{:v1 [:a]
                      :v2 [{:r "field"} :x]}]}

         {:desc     "hashmap mutation %s->%s add const into root without source"
          :spec     [:v1 :v2]
          :v1       {:a 1}
          :v2       {:a 1
                     :r "field"}
          :template [{:v1 []
                      :v2 [{:r "field"}]}]}]]
    (mapv (fn [{:keys [desc spec template] :as t}]
            (mapv (fn [[from to]]
                    (testing (format desc from to)
                      (is (= (to t)
                             (mutate (hmap-mutations [from to] template)
                                     (from t))))))
                  [spec (reverse spec)]))
          test-cases)))

(deftest test-real-data-roundtrip
  (let [test-cases
        [{:desc        "Appointments #%d roundtrip mutation %s-%s"
          :spec        [:stu3 :r4]
          :data-source (->> (load-edn "resources/private/apps.edn")
                            :entry
                            (map :resource))
          :template    [{:stu3 [:reason]
                         :r4   [:reasonCode]}
                        {:stu3 [:indication]
                         :r4   [:reasonReference]}
                        {:stu3 [:incomingReferral]
                         :r4   [:basedOn]}]}

         {:desc        "ClaimResponses #%d roundtrip mutation %s-%s"
          :spec        [:stu3 :r4]
          :data-source (load-edn "resources/private/clr.edn")
          :template    [{:stu3 [:requestProvider]
                         :r4   [:requestor]}
                        {:stu3 [[:item] :sequenceLinkId]
                         :r4   [[:item] :itemSequence]}
                        (comment ;; Input data doesn't conform next transformation, roundtrip will fail
                         {:stu3 [:outcome :coding [{:system "http://hl7.org/fhir/remittance-outcome"}] :code]
                          :r4   [:outcome]}) ; (when (#{"queued" "complete" "error" "partial"} :stu3.outcome.coding)
                        {:stu3 [:totalCost]
                         :r4   [:total :category :coding [{:code "submitted"}] :amount]}
                        {:stu3 [:totalBenefit]
                         :r4   [:total :category :coding [{:code "benefit"}] :amount]}]}]]

    (mapv (fn [{:keys [desc spec template data-source]}]
            (let [forwards-mut  (hmap-mutations spec template)
                  backwards-mut (hmap-mutations (reverse spec) template)]
              (doall
                (map-indexed
                  (fn [idx data]
                    (testing (apply format desc idx spec)
                      (is (= data
                             (->> data
                                  (mutate forwards-mut)
                                  (mutate backwards-mut))))))
                  data-source))))
          test-cases)))

(comment
  (run-tests)
  (comment))

