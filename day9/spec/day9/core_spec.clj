(ns day9.core-spec
  (:require [speclj.core :refer :all]
            [day9.core :refer :all]))

(describe "acceptance tests"
  (let [input
        (mapv #(Integer/parseInt %)
              (clojure.string/split-lines
                "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"))]

    (it "passes problem 1 acceptance test"
      (should= 127 (find-sum-error 5 input)))

    (it "passes problem 2 acceptance test"
      (should= 62 (find-encryption-weakness 127 input)))

    ))


(describe "tools"
  (it "should find combinations of sub sequence"
    (should= [[3 4]
              [3 5]
              [4 5]] (get-combinations 2 3 [1 2 3 4 5 6 7])))

  (it "Should find sums of combinations"
    (should= [7 8 9] (get-sums (get-combinations 2 3 [1 2 3 4 5 6 7]))))
  )

(describe "solvers"
  (it "should solve day 9 problem 1"
    (should= 4 (find-sum-error 2 [1 2 3 4 5 6 7]))
    )
  )

(describe "solutions"
  (let [input (slurp "input")
        input (clojure.string/split-lines input)
        input (mapv #(Long/parseLong %) input)]
    (it "solves the first problem"
      (should= 36845998 (find-sum-error 25 input)))

    (it "solves the second problem"
      (should= 4830226 (find-encryption-weakness 36845998 input))))

  )

