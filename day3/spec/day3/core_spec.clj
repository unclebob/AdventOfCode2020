(ns day3.core-spec
  (:require [speclj.core :refer :all]
            [day3.core :refer :all]))

(describe "reading input"
  (it "can translate a line"
    (should= [false true false] (translate-line ".#.\n")))

  (it "can translate multiple lines"
    (should= [[false true true] [true false true]]
             (translate-lines ".##\n#.#\n")))

  )

(describe "counting trees"
  (let [tree-map [[true false]
                  [false true]
                  [true true]]]
    (it "can count vertically"
      (should= 2 (count-trees tree-map [0 1]))
      (should= 3 (count-trees tree-map [1 1]))
      (should= 2 (count-trees tree-map [1 2]))
      )))

;(describe "debug"
;  (it "debug"
;    (let [input (slurp "debug")
;          tree-map (translate-lines input)
;          trees (count-trees tree-map [1 2])]
;      (should= 0 trees))
;    )
;  )


(describe "solution"
  (it "should be solved"
    (should= 0 (solve2)))
  )