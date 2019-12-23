(ns bxb.mutate.sql
  (:require [clojure.string :as str]
            [cheshire.core :as json]))

(defn kvs->jsidx [s] ; TODO: maybe place this into resolve-path
  (str "'{" (str/join \, s) "}'"))

(declare resolve-path)

(defn const-fn [constant]
  (constantly (json/generate-string constant)))

(defn search-fn [path value]
  (fn [data-source] (str "SELECT " (kvs->jsidx (resolve-path path data-source))
                         " FROM " data-source
                         " WHERE " ((const-fn value)))))

(declare map-fn)

(defn get-fn [path]
  (fn [data-source] (str data-source "#>>" (kvs->jsidx (resolve-path path data-source)))))

(defn assoc-fn [path get-value]
  (fn [data-source] (str "|| jsonb_set(" data-source ", " (kvs->jsidx (resolve-path path data-source)) ", " (get-value data-source) ")")))

(defn dissoc-fn
  ([path]
   (fn [data-source] (str "#- " (kvs->jsidx (resolve-path path data-source)))))
  ([path value]
   (dissoc-fn path))) ; TODO: match dissocing value with provided

(def fns
  {:const-fn  const-fn
   :search-fn search-fn
   :map-fn    map-fn
   :get-fn    get-fn
   :assoc-fn  assoc-fn
   :dissoc-fn dissoc-fn})
