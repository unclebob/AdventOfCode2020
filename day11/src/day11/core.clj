(ns day11.core
  (:require [clojure.string :as string]))

(defn parse-cell [char]
  (condp = char
    \L :empty
    \. :floor
    \# :person))

(defn parse-line [line]
  (mapv parse-cell line))

(defn parse-input [input]
  (let [lines (string/split-lines input)]
    (mapv parse-line lines)))

(defn get-cell [world row-index col-index]
  (let [row (nth world row-index)]
    (nth row col-index)))

(defn add-slope [[r c] [dr dc]]
  [(+ r dr) (+ c dc)])

(defn loc-out-of-bounds [[r c] world]
  (or
    (neg? r) (neg? c)
    (>= r (count world)) (>= c (count (first world)))))

(defn can-see-person [from slope world]
  (loop [loc (add-slope from slope)]
    (if (loc-out-of-bounds loc world)
      false
      (condp = (get-cell world (first loc) (second loc))
        :person true
        :empty false
        :floor (recur (add-slope loc slope))))))

  (defn count-visible-people [row col world]
    (let [directions [[0, 1] [1 1] [1 0] [-1 0]
                      [0 -1] [-1 -1] [1 -1] [-1 1]]
          visibles (map #(can-see-person [row col] % world) directions)]
      (count (filter identity visibles))))

  (defn count-neighbors [row col world]
    (let [row-count (count world)
          col-count (count (first world))
          neighbor-counts
          (for [r (range (dec row) (inc (inc row)))
                c (range (dec col) (inc (inc col)))]
            (cond
              (neg? r) 0
              (neg? c) 0
              (>= r row-count) 0
              (>= c col-count) 0
              (and (= r row) (= c col)) 0
              (= :person (get-cell world r c)) 1
              :else 0))]
      (reduce + neighbor-counts)))


  (defn update-automaton [world]
    (for [row (range (count world))]
      (for [col (range (count (nth world row)))]
        (condp = (get-cell world row col)
          :floor :floor
          :empty (if (= 0 (count-neighbors row col world)) :person :empty)
          :person (if (>= (count-neighbors row col world) 4) :empty :person)))))

  (defn update-automaton-2 [world]
    (for [row (range (count world))]
      (for [col (range (count (nth world row)))]
        (condp = (get-cell world row col)
          :floor :floor
          :empty (if (= 0 (count-visible-people row col world)) :person :empty)
          :person (if (>= (count-visible-people row col world) 5) :empty :person)))))

  (defn count-people [world]
    (count (filter #(= :person %) (flatten world))))

  (defn count-stable-people [world]
    (loop [world world last-world nil iterations 0]
      (if (= world last-world)
        (count-people world)
        (recur (update-automaton world) world (inc iterations)))))

  (defn count-stable-visible-people [world]
    (loop [world world last-world nil iterations 0]
      (if (= world last-world)
        (count-people world)
        (recur (update-automaton-2 world) world (inc iterations)))))
