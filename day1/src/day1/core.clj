(ns day1.core
  (:require [clojure.math.combinatorics :refer :all]))

(defn read-numbers [file]
  (let [number-file (slurp file)
        numbers-text (clojure.string/split number-file #"\n")]
    (map #(Integer/parseInt %) numbers-text)))

(defn find-pair [sum numbers]
  (let [pairs (combinations numbers 2)
        match (filter #(= sum (reduce + %)) pairs)]
    (first match)))

(defn solve []
  (let [numbers (read-numbers "input")
        [a b] (find-pair 2020 numbers)]
    [a b (* a b)]))

(defn solve2 []
  (let [numbers (read-numbers "input")
        tuples (combinations numbers 3)
        [a b c] (first (filter #(= 2020 (reduce + %)) tuples))]
    [a b c (* a b c)]))