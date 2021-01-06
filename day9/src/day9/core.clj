(ns day9.core
  (:require [clojure.math.combinatorics :as combo]))

(defn get-combinations [start n numbers]
  (let [sub (subvec numbers start (+ start n))]
    (combo/combinations sub 2)))

(defn get-sums [combinations]
  (map #(reduce + %) combinations))

(defn get-unmatched-sums [indices n numbers]
  (for [index indices]
    (let [start (- index n)
          combinations (get-combinations start n numbers)
          sums (get-sums combinations)
          number (nth numbers index)
          matches (filter #(= number %) sums)
          no-match? (empty? matches)]
      (if no-match? number nil))))

(defn find-sum-error [n numbers]
  (let [indices (range n (count numbers))
        unmatched (get-unmatched-sums indices n numbers)]
    (first (remove nil? unmatched))))

(defn find-encryption-weakness [n input]
  (let [indices (range 0 (dec (count input)))
        pairs (for [index indices
                    end (range (inc index) (count input))]
                [index end])
        spans (map #(subvec input (first %) (inc (second %))) pairs)
        sums (map #(list (reduce + %) %) spans)
        [_ span] (first (filter #(= n (first %)) sums))
        smallest (apply min span)
        largest (apply max span)]
    (+ smallest largest)))
