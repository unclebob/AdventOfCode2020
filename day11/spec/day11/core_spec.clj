(ns day11.core-spec
  (:require [speclj.core :refer :all]
            [day11.core :refer :all]))


(def test-data "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n")
(def test-data-step-1 "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##")
(def test-data-step-2 "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##")
(def test-data-step-3 "#.##.L#.##\n#L###LL.L#\nL.#.#..#..\n#L##.##.L#\n#.##.LL.LL\n#.###L#.##\n..#.#.....\n#L######L#\n#.LL###L.L\n#.#L###.##")
(def test-data-step-4 "#.#L.L#.##\n#LLL#LL.L#\nL.L.L..#..\n#LLL.##.L#\n#.LL.LL.LL\n#.LL#L#.##\n..L.L.....\n#L#LLLL#L#\n#.LLLLLL.L\n#.#L#L#.##")
(def test-data-step-5 "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##")

(def input (slurp "input"))

(describe "parser"
  (it "can parse one line"
    (should= [] (parse-line ""))
    (should= [:empty] (parse-line "L"))
    (should= [:empty :floor :person] (parse-line "L.#")))

  (it "should parse the whole input"
    (should= [[:empty :empty :floor]
              [:floor :empty :person]]
             (parse-input "LL.\n.L#\n"))))

(defn should-become [start-state end-state]
  (should= (parse-input end-state)
           (update-automaton (parse-input start-state))))

(describe "the automaton"
  (it "should occupy empty seats"
    (should-become "...\n.L.\n..." "...\n.#.\n...")
    (should-become ".#.\n.L.\n..." ".#.\n.L.\n...")
    (should-become "L..\n...\n..." "#..\n...\n...")
    (should-become "...\n.#.\n..." "...\n.#.\n..."))

  (it "should vacate surrounded seats"
    (should-become "#.#\n##.\n..#" "#.#\n#L.\n..#")
    (should-become "###\n###\n###" "#L#\nLLL\n#L#")
    )
  )

(describe "tools"
  (it "can see along a line"
    (should= true (can-see-person [1 1] [0 1]
                                  (parse-input
                                    "....\n.##.\n....")))
    (should= true (can-see-person [1 1] [0 1]
                                  (parse-input
                                    "....\n.#.#\n....")))
    (should= false (can-see-person [1 1] [0 1]
                                   (parse-input
                                     ".#..\n.#..\n....")))
    (should= true (can-see-person [0 0] [1 1]
                                  (parse-input
                                    "...\n...\n..#")))
    )

  (it "can count visibles"
    (should= 5 (count-visible-people
                 1 1
                 (parse-input
                   (str
                     ".#.\n"
                     "#.#\n"
                     "#.#"))))

    (should= 8 (count-visible-people
                 1 1
                 (parse-input
                   (str
                     "###\n"
                     "#.#\n"
                     "###")))))
  )

(defn should-become-2 [start-state end-state]
  (should= (parse-input end-state)
           (update-automaton-2 (parse-input start-state))))


(def test-data-step2-2 "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##")
(def test-data-step2-3 "#.LL.LL.L#\n#LLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLL#\n#.LLLLLL.L\n#.LLLLL.L#")
(def test-data-step2-4 "#.L#.##.L#\n#L#####.LL\nL.#.#..#..\n##L#.##.##\n#.##.#L.##\n#.#####.#L\n..#.#.....\nLLL####LL#\n#.L#####.L\n#.L####.L#")
(def test-data-step2-5 "#.L#.L#.L#\n#LLLLLL.LL\nL.L.L..#..\n##LL.LL.L#\nL.LL.LL.L#\n#.LLLLL.LL\n..L.L.....\nLLLLLLLLL#\n#.LLLLL#.L\n#.L#LL#.L#")
(def test-data-step2-6 "#.L#.L#.L#\n#LLLLLL.LL\nL.L.L..#..\n##L#.#L.L#\nL.L#.#L.L#\n#.L####.LL\n..#.#.....\nLLL###LLL#\n#.LLLLL#.L\n#.L#LL#.L#")
(def test-data-step2-7 "#.L#.L#.L#\n#LLLLLL.LL\nL.L.L..#..\n##L#.#L.L#\nL.L#.LL.L#\n#.LLLL#.LL\n..#.L.....\nLLL###LLL#\n#.LLLLL#.L\n#.L#LL#.L#")

(describe "Acceptance Tests"
  (it "should solve problem 1"
    (should-become test-data test-data-step-1)
    (should-become test-data-step-1 test-data-step-2)
    (should-become test-data-step-2 test-data-step-3)
    (should-become test-data-step-3 test-data-step-4)
    (should-become test-data-step-4 test-data-step-5)
    (should= 37 (count-people (parse-input test-data-step-5)))
    (should= 37 (count-stable-people (parse-input test-data))))

  (it "should solve problem 2"
    (should-become-2 test-data test-data-step2-2)
    (should-become-2 test-data-step2-2 test-data-step2-3)
    (should-become-2 test-data-step2-3 test-data-step2-4)
    (should-become-2 test-data-step2-4 test-data-step2-5)
    (should-become-2 test-data-step2-5 test-data-step2-6)
    (should-become-2 test-data-step2-6 test-data-step2-7)

    (should= 26 (count-stable-visible-people
                  (parse-input test-data)))

    )

  )


;(describe "Solutions"
;  (it "will solve problem 1"
;    (should= 2418 (count-stable-people (parse-input input))))
;
;  (it "will solve problem 2"
;    (should= 2144 (count-stable-visible-people (parse-input input)))))

