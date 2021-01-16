(ns day17.core-spec
  (:require [speclj.core :refer :all]
            [day17.core :refer :all]
            [clojure.string :as string]
            [clojure.set :as set]))


(defn parse-plane [input z]
  (let [lines (string/split-lines input)
        y (count lines)
        x (count (first lines))
        y (if (zero? x) 0 y)
        cell-states (for [ix (range x) iy (range y)] [(= \# (nth (nth lines iy) ix)) ix iy])
        active-cells (filter first cell-states)
        active-cells (map rest active-cells)
        active-cells (map #(conj (vec %) z) active-cells)]
    {:dimensions [x y]
     :map (into #{} active-cells)})
  )

(defn parse-input [input]
  (parse-plane input 0))

(defn neighbors-of [[x y z :as cell]]
  (let [neighbors (for [ix (range (- x 1) (+ x 2))
                        iy (range (- y 1) (+ y 2))
                        iz (range (- z 1) (+ z 2))]
                    [ix iy iz])
        neighbors (into #{} neighbors)
        neighbors (disj neighbors cell)]
    neighbors)
  )

(defn neighbor-count [cell world]
  (let [neighbors (neighbors-of cell)
        active-neighbors (set/intersection neighbors world)]
    (count active-neighbors))
  )

(defn remove-cell [cell world]
  (let [n (neighbor-count cell world)]
    (if (or (> n 3) (< n 2))
      cell
      nil)
    ))

(defn get-cells-for-removal [world]
  (let [removed-cells (for [cell world]
                        (remove-cell cell world))
        removed-cells (filter some? removed-cells)]
    (into #{} removed-cells)))

(defn get-bounds [world]
  (let [xs (map first world)
        ys (map second world)
        zs (map #(nth % 2) world)
        xs (if (empty? xs) [0] xs)
        ys (if (empty? ys) [0] ys)
        zs (if (empty? zs) [0] zs)]
    [[(apply max xs) (apply min xs)]
     [(apply max ys) (apply min ys)]
     [(apply max zs) (apply min zs)]]))

(defn add-cell [cell world]
  (let [n (neighbor-count cell world)]
    (if (= n 3) cell nil))
  )

(defn get-new-cells [world]
  (let [[[max-x min-x]
         [max-y min-y]
         [max-z min-z]] (get-bounds world)
        max-x (inc max-x)
        min-x (dec min-x)
        max-y (inc max-y)
        min-y (dec min-y)
        max-z (inc max-z)
        min-z (dec min-z)
        added-cells (for [ix (range min-x (inc max-x))
                          iy (range min-y (inc max-y))
                          iz (range min-z (inc max-z))]
                      (add-cell [ix iy iz] world))
        added-cells (filter some? added-cells)
        ]
    (into #{} added-cells)
    )
  )

(defn do-turn [world]
  (let [removed (get-cells-for-removal world)
        added (get-new-cells world)]
    (set/union added (set/difference world removed)))
  )

(defn do-turns [n world]
  (loop [n n world world]
    (if (zero? n)
      world
      (recur (dec n) (do-turn world)))))

(defn solve-1 []
  (let [input (slurp "input")]
    (count (do-turns 6 (:map (parse-input input))))))

(describe "Day 17"
  (context "Parsing"
    (it "should be able to get the size of the input"
      (should= [2 3] (:dimensions (parse-input "..\n..\n..\n")))
      (should= [0 0] (:dimensions (parse-input "")))
      )
    (it "should be able to build a map of cells"
      (should= #{} (:map (parse-input "")))
      (should= #{} (:map (parse-input "..\n..\n..\n")))
      (should= #{[0 1 0] [1 1 0]} (:map (parse-input "..\n##\n..\n")))
      (should= #{[3 0 0] [1 1 0] [2 1 0] [0 0 0]} (:map (parse-input "#..#\n.##."))))
    )

  (context "utilities"
    (it "should find 26 neighbors"
      (should= 26 (count (neighbors-of [0 0 0])))
      (should= #{[0 1 0] [1 0 2] [0 1 1]
                 [0 0 2] [2 0 2] [1 0 0]
                 [1 2 1] [0 1 2] [0 0 1]
                 [2 2 2] [2 2 0] [0 2 0]
                 [2 2 1] [1 2 0] [2 1 1]
                 [1 1 0] [1 1 2] [2 0 0]
                 [0 2 2] [2 1 2] [2 1 0]
                 [1 2 2] [1 0 1] [0 0 0]
                 [2 0 1] [0 2 1]} (neighbors-of [1 1 1])))

    (it "should find active neighbor count"
      (should= 0 (neighbor-count [0 0 0] #{}))
      (should= 1 (neighbor-count [0 0 0] #{[1 1 1]}))
      (should= 1 (neighbor-count [0 0 0] #{[-1 -1 -1]}))
      (should= 2 (neighbor-count [0 0 0] #{[1 1 1] [1 0 -1]})))

    (it "should remove cells with less than two or more than three neighbors"
      (should= #{} (get-cells-for-removal #{}))
      (should= #{[0 0 0]} (get-cells-for-removal #{[0 0 0]}))
      (should= #{[0 0 0] [2 0 0]} (get-cells-for-removal (:map (parse-input "###"))))
      (should= #{[1 0 0] [1 1 0]} (get-cells-for-removal (:map (parse-input "###\n###")))))

    (it "should find the bounds of the world"
      (should= [[0 0] [0 0] [0 0]] (get-bounds #{}))
      (should= [[1 1] [1 1] [1 1]] (get-bounds #{[1 1 1]}))
      (should= [[3 1] [3 1] [3 1]] (get-bounds #{[1 1 1] [3 3 3]}))

      )

    (it "should add cells with 3 neighbors"
      (should= #{} (get-new-cells #{}))
      (should= #{[1 -1 -1] [1 -1 1] [1 -1 0]
                 [1 1 1] [1 1 -1] [1 1 0]
                 [1 0 1] [1 0 -1]} (get-new-cells (:map (parse-input "###")))))
    )
  (def input (:map (parse-input ".#.\n..#\n###")))

  (context "Acceptance Tests"
    (it "should pass part 1"
      (should= 11 (count (do-turn input)))
      (should= 21 (count (do-turns 2 input)))
      (should= 112 (count (do-turns 6 input))))
    )

  (context "Solutions"
    (it "should solve part 1"
      (should= 298 (solve-1))))
  )