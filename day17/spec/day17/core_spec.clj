(ns day17.core-spec
  (:require [speclj.core :refer :all]
            [day17.core :refer :all]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb]))

(defn parse-plane [input dimensions]
  (let [lines (string/split-lines input)
        y (count lines)
        x (count (first lines))
        y (if (zero? x) 0 y)
        cell-states (for [ix (range x) iy (range y)] [(= \# (nth (nth lines iy) ix)) ix iy])
        active-cells (filter first cell-states)
        active-cells (map rest active-cells)
        active-cells (map #(concat (vec %) (repeat (- dimensions 2) 0)) active-cells)]
    (into #{} active-cells)))

(defn parse-input [input dimensions]
  (parse-plane input dimensions))

(defn- add2 [n] (+ 2 n))

(defn get-neighbors [cell]
  (let [ranges (map #((juxt dec add2) %) cell)
        ranges (map #(apply range %) ranges)
        neighbors (apply comb/cartesian-product ranges)
        neighbors (into #{} neighbors)
        neighbors (disj neighbors cell)]
    neighbors))


(defn count-active-neighbors [cell world]
  (let [neighbors (get-neighbors cell)
        active-neighbors (set/intersection neighbors world)]
    (count active-neighbors)))

(defn remove-cell [cell world]
  (let [n (count-active-neighbors cell world)]
    (if (or (> n 3) (< n 2))
      cell
      nil)))

(defn get-cells-for-removal [world]
  (let [removed-cells (for [cell world]
                        (remove-cell cell world))
        removed-cells (filter some? removed-cells)]
    (into #{} removed-cells)))

(defn add-cell [cell world]
  (let [n (count-active-neighbors cell world)]
    (if (= n 3) cell nil)))


(defn get-new-cells [world]
  (let [neighbors (set (mapcat get-neighbors world))
        added-cells (map #(add-cell % world) neighbors)
        added-cells (filter some? added-cells)]
    (into #{} added-cells)))

(defn get-next-state [world]
  (let [removed (get-cells-for-removal world)
        added (get-new-cells world)]
    (set/union added (set/difference world removed))))

(defn get-next-states [n world]
  (loop [n n world world]
    (if (zero? n)
      world
      (recur (dec n) (get-next-state world)))))

(defn solve-1 []
  (let [input (slurp "input")]
    (count (get-next-states 6 (parse-input input 3)))))

(defn solve-2 []
  (let [input (slurp "input")]
    (count (get-next-states 6 (parse-input input 4)))))

(describe "Day 17"
  (context "Parsing"
    (it "should be able to build a map of cells"
      (should= #{} (parse-input "" 3))
      (should= #{} (parse-input "..\n..\n..\n" 3))
      (should= #{[0 1 0] [1 1 0]} (parse-input "..\n##\n..\n" 3))
      (should= #{[3 0 0] [1 1 0] [2 1 0] [0 0 0]} (parse-input "#..#\n.##." 3))))

  (context "utilities"
    (it "should find 26 neighbors"
      (should= 26 (count (get-neighbors [0 0 0])))
      (should= #{[0 1 0] [1 0 2] [0 1 1]
                 [0 0 2] [2 0 2] [1 0 0]
                 [1 2 1] [0 1 2] [0 0 1]
                 [2 2 2] [2 2 0] [0 2 0]
                 [2 2 1] [1 2 0] [2 1 1]
                 [1 1 0] [1 1 2] [2 0 0]
                 [0 2 2] [2 1 2] [2 1 0]
                 [1 2 2] [1 0 1] [0 0 0]
                 [2 0 1] [0 2 1]} (get-neighbors [1 1 1])))

    (it "should find active neighbor count"
      (should= 0 (count-active-neighbors [0 0 0] #{}))
      (should= 1 (count-active-neighbors [0 0 0] #{[1 1 1]}))
      (should= 1 (count-active-neighbors [0 0 0] #{[-1 -1 -1]}))
      (should= 2 (count-active-neighbors [0 0 0] #{[1 1 1] [1 0 -1]})))

    (it "should remove cells with less than two or more than three neighbors"
      (should= #{} (get-cells-for-removal #{}))
      (should= #{[0 0 0]} (get-cells-for-removal #{[0 0 0]}))
      (should= #{[0 0 0] [2 0 0]} (get-cells-for-removal (parse-input "###" 3)))
      (should= #{[1 0 0] [1 1 0]} (get-cells-for-removal (parse-input "###\n###" 3))))

    (it "should add cells with 3 neighbors"
      (should= #{} (get-new-cells #{}))
      (should= #{[1 -1 -1] [1 -1 1] [1 -1 0]
                 [1 1 1] [1 1 -1] [1 1 0]
                 [1 0 1] [1 0 -1]} (get-new-cells (parse-input "###" 3)))))

  (def input (parse-input ".#.\n..#\n###" 3))

  (context "Acceptance Tests"
    (it "should pass part 1"
      (should= 11 (count (get-next-state input)))
      (should= 21 (count (get-next-states 2 input)))
      (should= 112 (count (get-next-states 6 input)))))

  (context "Solutions"
    (it "should solve part 1"
      (should= 298 (solve-1)))

    (it "should solve part 2"
      (should= 1792 (solve-2)))))