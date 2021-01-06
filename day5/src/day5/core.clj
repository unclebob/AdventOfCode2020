(ns day5.core
  (:require [clojure.set :as s]))

(defn to-bit [char]
  (condp = char
    \F \0
    \L \0
    \1))

(defn seat-id [code]
  (let [b-string (apply str (map to-bit code))]
    (Integer/parseInt b-string 2))
  )

(defn solve-1 []
  (let [input (slurp "input")
        boarding-passes (clojure.string/split input #"\n")
        seat-ids (map seat-id boarding-passes)
        ]
    (apply max seat-ids)))

(defn solve-2 []
  (let [input (slurp "input")
        boarding-passes (clojure.string/split input #"\n")
        seat-ids (sort (map seat-id boarding-passes))
        possible-seats (range (first seat-ids) (last seat-ids))
        seat (s/difference (set possible-seats) (set seat-ids))
        ]
    (first seat))
  )
