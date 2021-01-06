(ns day15.core-spec
  (:require [speclj.core :refer :all]
            [day15.core :refer :all]))

(defn solve [n input]
  (let [tokens (clojure.string/split input #",")
        seeds (map #(Integer/parseInt %) tokens)
        seed-pairs (map vector seeds (range 1 (count seeds)))
        turn-map (into (hash-map) seed-pairs)
        ]
    (loop [turn (count seeds)
           last (last seeds)
           turn-map turn-map]
      (let [last-time (get turn-map last 0)
            number (if (zero? last-time)
                     0
                     (- turn last-time))]
        (if (= n turn)
          last
          (recur (inc turn)
                 number
                 (assoc turn-map last turn))))))
  )

(describe "Day 15: Number sequences"
  (context "Part 1"
    (it "Solves acceptance tests."
      (should= 436 (solve 2020 "0,3,6"))
      (should= 1 (solve 2020 "1,3,2"))
      (should= 10 (solve 2020 "2,1,3"))
      (should= 1836 (solve 2020 "3,1,2")))

    (it "Solves Part 1"
      (should= 253 (solve 2020 "18,8,0,5,4,1,20"))))

  (context "Part 2"
    (it "Passes acceptance tests"
      (should= 175594 (solve 30000000 "0,3,6")))

    (it "Solves Part 2"
      (should= 13710 (solve 30000000 "18,8,0,5,4,1,20"))))
  )
