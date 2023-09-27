(ns fwpd.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))
(def entry {:name "Leo" :glitter-index 9})
(def entries {:entry1 {:name "Leo" :glitter-index 9}
              :entry2 {:name "Alice" :glitter-index 7}
              :entry3 {:name "Bob" :glitter-index 12}})
(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  (let [trimmed-value (clojure.string/trim value)]
    (if (empty? trimmed-value)
      nil ; Retornar nil se a string estiver vazia após a remoção dos caracteres não numéricos
      (case vamp-key
        :glitter-index (Integer. trimmed-value)
        :name trimmed-value))))


(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(def map->name (partial map :name))

(defn glitter-filter-new
  [minimum-glitter records]
  (map->name (glitter-filter minimum-glitter records)))

(defn validate
  [x y]
  (= (set (keys x)) (set (keys y))))

(defn append
  "Append new suspect to the suspect list."
  [coll x]
  (if (validate conversions x)
    (concat coll (seq [x]))
    coll))

(defn suspect->str
  [record]
  (clojure.string/join "," [(:name record) (:glitter-index record)]))

(defn suspects->str
  [records]
  (clojure.string/join "\n" (map suspect->str records)))


(defn to-csv
  "Grava uma entrada em um arquivo CSV"
  [filename entry]
  (with-open [writer (io/writer filename :append true)]
    (csv/write-csv writer [[(:name entry) (:glitter-index entry)]])))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [filename "suspects.csv"
        records (mapify (parse (slurp filename)))]
    (glitter-filter-new 3 records)
    (append records entry)
    (validate conversions entry)
    (println(suspect->str entry))
    (to-csv filename entry )))