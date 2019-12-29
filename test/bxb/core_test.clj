(ns bxb.core-test
  (:require [clojure.test      :refer [deftest is testing]]
            [bxb.core          :refer [mutate hmap-transformations sql-transformations]]
            [bxb.misc          :refer [load-edn]]
            [clojure.java.jdbc :as db]
            [honeysql.core     :as hsql]
            [cheshire.core     :as json])
  (:import org.postgresql.util.PGobject))

;; setup JSON fields to automatically serialize-deserialize
(defn value-to-json-pgobject [value]
  (doto (PGobject.)
    ;; hack for now -- eventually we should properly determine the actual type
    (.setType "jsonb")
    (.setValue (json/generate-string value))))

(extend-protocol db/ISQLValue
  clojure.lang.IPersistentMap
  (sql-value [value] (value-to-json-pgobject value)))

(extend-protocol db/IResultSetReadColumn
  org.postgresql.util.PGobject
  (result-set-read-column [pgobj metadata idx]
    (let [type (.getType pgobj)
          value (.getValue pgobj)]
      (if (#{"jsonb" "json"} type)
        (json/parse-string value true)
        value))))

(def test-cases
  [{:desc    "hashmap mutation %s->%s nested name remap"
    :spec    [:stu3 :r4]
    :stu3    {:resourceType "VisionPrescription"
              :reason       "ups"
              :dispense     {:prism 5
                             :base  1}}
    :r4      {:resourceType "VisionPrescription"
              :code         "ups"
              :dispense     {:amount 5
                             :base   1}}
    :mapping [{:stu3 [:reason]
               :r4   [:code]}
              {:stu3 [:dispense :prism]
               :r4   [:dispense :amount]}
              {:stu3 [:dispense :base]
               :r4   [:dispense :base]}]}

   {:desc    "hashmap mutation %s->%s with array destruction"
    :spec    [:stu3 :r4]
    :stu3    {:resourceType "VisionPrescription"
              :dispense     [{:prism 5}]}
    :r4      {:resourceType "VisionPrescription"
              :amount       5}
    :mapping [{:stu3 [:dispense 0 :prism]
               :r4   [:amount]}]}

   {:desc    "hashmap mutation %s->%s (un)nesting name remap"
    :spec    [:stu3 :r4]
    :stu3    {:resourceType "VisionPrescription"
              :reason       "ups"
              :dispense     {:prism 5
                             :base  1}}
    :r4      {:resourceType "VisionPrescription"
              :reason       {:code "ups"}
              :amount       5
              :base         1}
    :mapping [{:stu3 [:reason]
               :r4   [:reason :code]}
              {:stu3 [:dispense :prism]
               :r4   [:amount]}
              {:stu3 [:dispense :base]
               :r4   [:base]}]}

   {:desc    "hashmap mutation %s->%s with adding const by the path"
    :spec    [:stu3 :r4]
    :stu3    {:resourceType "VisionPrescription"
              :reason       "ups"}
    :r4      {:resourceType "VisionPrescription"
              :extension    {:url   "http://hl7.org/reason"
                             :value {:code "ups"}}}
    :mapping [{:stu3 [:reason]
               :r4   [:extension {:url "http://hl7.org/reason"} :value :code]}]}

   {:desc    "hashmap mutation forwards %s->%s with creating extension vec"
    :spec    [:stu3 :r4]
    :stu3    {:resourceType "VisionPrescription"
              :reason       "ups"}
    :r4      {:resourceType "VisionPrescription"
              :extension    [{:url   "http://hl7.org/reason"
                              :value {:code "ups"}}]}
    :mapping [{:stu3 [:reason]
               :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}]}

   {:desc    "hashmap mutation forwards %s->%s with existing extension vec"
    :spec    [:stu3 :r4]
    :stu3    {:resourceType "VisionPrescription"
              :reason       "ups"
              :extension    [{:url   "http://hl7.org/fact"
                              :value {:code "fucct"}}]}
    :r4      {:resourceType "VisionPrescription"
              :extension    [{:url   "http://hl7.org/fact"
                              :value {:code "fucct"}}
                             {:url   "http://hl7.org/reason"
                              :value {:code "ups"}}]}
    :mapping [{:stu3 [:reason]
               :r4   [:extension [{:url "http://hl7.org/reason"}] :value :code]}]}

   {:desc    "hashmap mutation %s->%s remap with missing source"
    :spec    [:stu3 :r4]                          ;; TODO: дефолт значения
    :stu3    {:resourceType "VisionPrescription"} ;; :^default{:value 0}
    :r4      {:resourceType "VisionPrescription"}
    :mapping [{:stu3 [:dispense :prism]
               :r4   [:lensSpecification :amount]}]}

   {:desc    "hashmap mutation %s->%s mapped name remap"
    :spec    [:v1 :v2]
    :v1      {:i {:a [{:b 1, :c -1} {:b 2, :c -2}]}}
    :v2      {:i {:a [{:y 1, :c -1} {:y 2, :c -2}]}}
    :mapping [{:v1 [:i [:a] :b]
               :v2 [:i [:a] :y]}]}

   {:desc    "hashmap mutation %s->%s mapped name remap with missing source"
    :spec    [:v1 :v2]
    :v1      {:i {:not-a 0}}
    :v2      {:i {:not-a 0}}
    :mapping [{:v1 [:i [:a] :b]
               :v2 [:i [:a] :y]}]}

   {:desc    "hashmap mutation %s->%s root array mapped name remap"
    :spec    [:v1 :v2]
    :v1      {:a [{:b 1, :c -1} {:b 2, :c -2}]}
    :v2      {:x [{:y 1, :c -1} {:y 2, :c -2}]}
    :mapping [{:v1 [[:a] :b]
               :v2 [[:x] :y]}]}

                                        ;{:desc     "hashmap mutation %s->%s filtered mapped name remap"
                                        ; :spec     [:v1 :v2]
                                        ; :v1       {:i {:a [{:b 1, :flag true} {:b 2, :flag false}]}}
                                        ; :v2       {:i {:x [{:y 1, :flag true} {:b 2, :flag false}]}}
                                        ; :mapping [{:v1 [:i [:a] [{:flag true}] :b]     ;;TODO: продумать синтаксис матчинга
                                        ;             :v2 [:i [:x] [{:flag true}] :y]}]}  ;; может быть, мета-ключем

   {:desc    "hashmap mutation %s->%s nested map name remap"
    :spec    [:v1 :v2]
    :v1      {:a [{:b [{:c 1} {:c 2}]}
                  {:b [{:c 3} {:c 4}]}]}
    :v2      {:x [{:y [{:z 1} {:z 2}]}
                  {:y [{:z 3} {:z 4}]}]}
    :mapping [{:v1 [[:a] [:b] :c]
               :v2 [[:x] [:y] :z]}]}

   {:desc    "hashmap mutation %s->%s add const into root"
    :spec    [:v1 :v2]
    :v1      {:a 1}
    :v2      {:x 1
              :r "field"}
    :mapping [{:v1 [:a]
               :v2 [{:r "field"} :x]}]}

   {:desc    "hashmap mutation %s->%s add const into root without source"
    :spec    [:v1 :v2]
    :v1      {:a 1}
    :v2      {:a 1
              :r "field"}
    :mapping [{:v1 []
               :v2 [{:r "field"}]}]}

   {:desc    "hashmap mutation %s->%s (un)nesting rename"
    :spec    [:v1 :v2]
    :v1      {:a 1}
    :v2      {:a {:b 1}}
    :mapping [{:v1 [:a]
               :v2 [:a :b]}]}])

(def real-cases
  [{:desc        "Appointments #%d roundtrip mutation %s-%s"
    :spec        [:stu3 :r4]
    :data-source (->> (load-edn "resources/apps.edn")
                      :entry
                      (map :resource))
    :mapping     [{:stu3 [:reason]
                   :r4   [:reasonCode]}
                  {:stu3 [:indication]
                   :r4   [:reasonReference]}
                  {:stu3 [:incomingReferral]
                   :r4   [:basedOn]}]}

   {:desc        "ClaimResponses #%d roundtrip mutation %s-%s"
    :spec        [:stu3 :r4]
    :data-source (load-edn "resources/clr.edn")
    :mapping     [{:stu3 [:requestProvider]
                   :r4   [:requestor]}
                  {:stu3 [[:item] :sequenceLinkId]
                   :r4   [[:item] :itemSequence]}
                  {:stu3 [:outcome :coding [{:system "http://hl7.org/fhir/remittance-outcome"}] :code]
                   :r4   [:outcome]} ; (when (#{"queued" "complete" "error" "partial"} :stu3.outcome.coding)
                  {:stu3 [:totalCost]
                   :r4   [:total [{:category {:coding [{:code "submitted"}]}}] :amount]}
                  {:stu3 [:totalBenefit]
                   :r4   [:total [{:category {:coding [{:code "benefit"}]}}] :amount]}]}

   {:desc        "Fhir ClaimResponses #%d roundtrip mutation %s-%s"
    :spec        [:r4 :stu3]
    :data-source (json/parse-string (slurp "resources/fhir_claimresponse.json") keyword)
    :mapping     [{:stu3 [:requestProvider]
                   :r4   [:requestor]}
                  {:stu3 [[:item] :sequenceLinkId]
                   :r4   [[:item] :itemSequence]}
                  {:stu3 [:outcome :coding [{:system "http://hl7.org/fhir/remittance-outcome"}] :code]
                   :r4   [:outcome]} ; (when (#{"queued" "complete" "error" "partial"} :stu3.outcome.coding)
                  {:stu3 [:totalCost]
                   :r4   [:total [{:category {:coding [{:code "submitted"}]}}] :amount]}
                  {:stu3 [:totalBenefit]
                   :r4   [:total [{:category {:coding [{:code "benefit"}]}}] :amount]}]}])

(deftest hmap
  (doseq [{:keys [desc spec mapping] :as t} test-cases]
    (doseq [[from to] [spec (reverse spec)]]
      (testing (format desc from to)
        (is (= (to t)
               (mutate (hmap-transformations [from to] mapping)
                       (from t)))))
      (testing (str "Round trip " (format desc from to))
        (is (= (from t)
               (->> (from t)
                    (mutate (hmap-transformations [from to] mapping))
                    (mutate (hmap-transformations [to from] mapping))))))))

  (doseq [{:keys [desc spec mapping data-source]} real-cases]
    (let [forwards-mut  (hmap-transformations spec mapping)
          backwards-mut (hmap-transformations (reverse spec) mapping)]
      (doseq [[idx data] (map-indexed vector data-source)]
        (testing (apply format desc idx spec)
          (is (= data
                 (->> data
                      (mutate forwards-mut)
                      (mutate backwards-mut)))))))))

(deftest sql

  (def db
    {:dbtype   "postgresql"
     :dbname   "test_testbox"
     :host     "localhost"
     :port     "5439"
     :user     "postgres"
     :password "postgres"})

  (defn eval-mut-in-pg [transformations data]
    (->> {:select [[(mutate transformations
                            (hsql/call :cast (json/generate-string data) :jsonb)) :r]]}
         hsql/format
         (db/query db)
         first
         :r))

  (doseq [{:keys [desc spec mapping] :as t} test-cases]
    (doseq [[from to] [spec (reverse spec)]]
      (testing (format desc from to)
        (is (= (to t)
               (eval-mut-in-pg (sql-transformations [from to] mapping) (from t)))))
      (testing (str "Round trip " (format desc from to))
        (is (= (from t)
               (->> (from t)
                    (eval-mut-in-pg (sql-transformations [from to] mapping))
                    (eval-mut-in-pg (sql-transformations [to from] mapping))))))))


  (doseq [{:keys [desc spec mapping data-source]} real-cases]
    (let [forwards-mut  (sql-transformations spec mapping)
          backwards-mut (sql-transformations (reverse spec) mapping)]
      (doseq [[idx data] (map-indexed vector data-source)]
        (testing (apply format desc idx spec)
          (is (= data
                 (->> data
                      (eval-mut-in-pg forwards-mut)
                      (eval-mut-in-pg backwards-mut)))))))))
