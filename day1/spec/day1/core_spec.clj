(ns day1.core-spec
  (:require [speclj.core :refer :all]
            [day1.core :refer :all]))

(describe "reading input"
  (it "can read a file of numbers"
    (spit "t-input" "1\n2\n3\n")
    (should= [1 2 3] (read-numbers "t-input"))
    ))

(describe "finding pairs that add to"
  (it "can find a pair"
    (should= [0 0] (find-pair 0 [0 0]))
    (should= [0 1] (find-pair 1 [0 1 2]))
    )
  )

(describe "solution"
  (it "is"
    (should= nil (solve2))))
