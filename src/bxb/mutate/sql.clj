(ns bxb.mutate.sql
  (:require [clojure.string :as str]
            [honeysql.core    :as hsql]
            [cheshire.core :as json]))

(defn kvs->jsidx [s] ; TODO: maybe place this into resolve-path
  (str "'{" (str/join \, s) "}'"))

(defn resolve-path [path src]
  (mapv (fn [pt] (pt src))
        path))

(defn const-fn [constant]
  (constantly ((if (keyword? constant) name str) constant)))

(defn search-fn [path value]
  (fn [src dest] "search"))

(declare map-fn)

(defn get-fn [path]
  (fn [src dest]
    (hsql/call
     "#>"
     src
     (kvs->jsidx (resolve-path path src)))))

(defn assoc-fn [path get-value]
  (fn [src dest]
    (loop [path   #spy/p (resolve-path path src)
           result #spy/p (get-value src dest)]
      (if-let [lp (peek path)]
        (let [path (pop path)]
          (recur path
                 (hsql/call :jsonb_set
                            (if (seq path)
                              (hsql/call :coalesce
                                         (hsql/call "#>" :resource (kvs->jsidx path))
                                         "'{}'::jsonb")
                              dest)
                            (str \' \{ lp \} \')
                            result)))
        result))))

(defn dissoc-fn
  ([path]
   (fn [src dest]
     (hsql/call
      "#-"
      dest
      (kvs->jsidx (resolve-path path dest)))))
  ([path value]
   (dissoc-fn path))) ; TODO: match dissocing value with provided

(def fns
  {:const-fn  const-fn
   :search-fn search-fn
   :map-fn    map-fn
   :get-fn    get-fn
   :assoc-fn  assoc-fn
   :dissoc-fn dissoc-fn})
