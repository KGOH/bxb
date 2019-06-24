(ns bxb.mutate-fns
  (:require [clojure.string :as str]
            [bxb.misc :refer [dissoc-in p]]
            [cheshire.core :as json]))

(defn hmap-get-fn [path]
  #(get-in % path))

(defn hmap-assoc-fn [path get-value]
  #(assoc-in % path (get-value %)))

(defn hmap-dissoc-fn
 ([path]
  #(dissoc-in % path))
 ([path & paths]
  #(apply dissoc-in % path paths)))

(defn kws->jsidx [s]
  (str "'{" (str/join \, (map json/generate-string s)) "}'"))

(defn sql-get-fn [path]
  (fn [_] (str "resource#>>" (kws->jsidx path))))

(defn sql-assoc-fn [path get-value]
  (fn [_] (str "|| jsonb_set(resource, " (kws->jsidx path) ", " (get-value _) ")")))

(defn sql-dissoc-fn
 ([path]
  (fn [_] (str "#- " (kws->jsidx path))))
 ([path & paths]
  (fn [_] (str/join \space (map #(str "#- " (kws->jsidx %)) (conj paths path))))))
