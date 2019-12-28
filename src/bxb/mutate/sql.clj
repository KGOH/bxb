(ns bxb.mutate.sql
  (:require [clojure.string :as str]
            [honeysql.core    :as hsql]
            [cheshire.core :as json]))

(defn kvs->jsidx [s] ; TODO: maybe place this into resolve-path
  (str "'{" (str/join \, s) "}'"))

(defn resolve-path [path data-source]
  (mapv (fn [pt] (pt data-source))
        path))

(defn const-fn [constant]
  (constantly ((if (keyword? constant) name str) constant)))

(defn search-fn [path value]
  (fn [data-source] "search"))

(declare map-fn)

(defn get-resource [data-source]
  (:set data-source :resource))

(defn resource [r]
  (or r :resource))

(defn get-fn [path]
  (fn [data-source]
    (hsql/call
     "#>"
     data-source
     (kvs->jsidx (resolve-path path data-source)))))

(defn assoc-fn [path get-value]
  (fn [data-source]
    (loop [path   (resolve-path path data-source)
           result (get-value data-source)]
      (if-let [lp (last path)]
        (let [path (butlast path)]
          (recur path
                 (hsql/call :jsonb_set
                            (if (seq path)
                              (hsql/call :coalesce
                                         (hsql/call "#>" :resource (kvs->jsidx path))
                                         "'{}'::jsonb")
                              :resource)
                            (str \' \{ lp \} \')
                            result)))
        result))))

(defn dissoc-fn
  ([path]
   (fn [data-source]
     (hsql/call
      "#-"
      data-source
      (kvs->jsidx (resolve-path path data-source)))))
  ([path value]
   (dissoc-fn path))) ; TODO: match dissocing value with provided

(def fns
  {:const-fn  const-fn
   :search-fn search-fn
   :map-fn    map-fn
   :get-fn    get-fn
   :assoc-fn  assoc-fn
   :dissoc-fn dissoc-fn})
