(ns day13.core-spec
  (:require [speclj.core :refer :all]
            [day13.core :refer :all]
            [clojure.string :as string]))

(def test-data "939\n7,13,x,x,59,x,31,19")

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        earliest (Integer/parseInt (first lines))
        all-buses (clojure.string/split (second lines) #",")
        buses (map #(Integer/parseInt %) (remove #(= "x" %) all-buses))]
    [earliest buses])
  )

(defn time-before-departure [earliest bus]
  [bus (- bus (mod earliest bus))]
  )

(defn find-best-bus [[earliest buses]]
  (let [times-before-departure (map #(time-before-departure earliest %) buses)
        earliest-bus (first (sort-by second times-before-departure))]
    (reduce * earliest-bus)))

(describe "day 13 part 1"
  (it "should parse input"
    (should= [939 [7 13 59 31 19]] (parse-input test-data)))

  (it "should pass acceptance test"
    (should= 295 (find-best-bus (parse-input test-data))))

  (it "should solve problem"
    (should= 3246 (find-best-bus (parse-input (slurp "input")))))
  )

(defn is-prime? [n]
  (if (= n 2)
    true
    (let [divisors (range 2 (dec n))
          remainders (map #(mod n %) divisors)
          zeroes (filter zero? remainders)]
      (= 0 (count zeroes))))
  )

(defn get-bus-specs [input]
  (let [lines (string/split-lines input)
        bus-specs (string/split (second lines) #",")
        indexed-specs (map #(vector %1 %2)
                           (range (inc (count bus-specs)))
                           bus-specs)
        indexed-specs (remove #(= "x" (second %)) indexed-specs)
        indexed-buses (map #(vector (first %) (Integer/parseInt (second %))) indexed-specs)

        ]
    indexed-buses
    ))

(let [input (parse-input (slurp "input"))]
  (describe "research"
    (it "should find max bus"
      (should= 983 (apply max (second input))))

    (it "should find primes"
      (should (is-prime? 2))
      (should (is-prime? 3))
      (should (is-prime? 5))
      (should (is-prime? 7))
      (should (is-prime? 11))
      (should-not (is-prime? 4))
      (should-not (is-prime? 6))
      (should-not (is-prime? 8))
      (should-not (is-prime? 9)))

    (it "should determine if all busses are prime"
      (let [busses (second input)
            prime-tests (map is-prime? busses)
            trues (filter identity prime-tests)]
        (should= (count busses) (count trues))))

    (it "should find when busses are all back to zero.  The modulus limit."
      (should= 2259477766995683 (apply * (second input))))

    (it "should print graph of simple problem"
      (let [bus-specs (get-bus-specs "xx\n2,3,5")
            rows (apply * (map second bus-specs))
            indices (range rows)
            lines (apply str
                         (for [n indices]
                           (str (format "\n%2d:" n)
                                (apply str
                                       (map #(if (zero?
                                                    (mod (+ n (first %)) (second %)))
                                               "X"
                                               " ") bus-specs)))))
            ]
        (println lines)
        (should= nil nil)))
    ))

(defn find-bus-time [bus-specs]
  (loop [time 0
         increment 1
         [[offset bus] & specs :as bus-specs] bus-specs]
    (cond (empty? bus-specs) time
          (zero? (mod (+ time offset) bus)) (recur time (* increment bus) specs)
          :else (recur (+ time increment) increment bus-specs)))
  )

(describe "solving part 2, the modulus problem"
  (context "parsing the input"
    (it "should get the bus specs"
      ;(def test-data "939\n7,13,x,x,59,x,31,19")
      (should= [[0 7] [1 13] [4 59] [6 31] [7 19]] (get-bus-specs test-data)))
    )
  (context "Acceptance Tests"
    (it "should pass simple test"
      (should= 8 (find-bus-time (get-bus-specs "xx\n2,3,5"))))
    (it "should pass sample test 1"
      (should= 3417 (find-bus-time (get-bus-specs "xx\n17,x,13,19"))))
    (it "should pass final acceptance test"
      (should= 1068781 (find-bus-time (get-bus-specs test-data))))
    )

  (context "Solve problem 2"
    (it "solves problem 2"
      (let [input (slurp "input")
            buses (get-bus-specs input)]
        (should= 1010182346291467 (find-bus-time buses)))))
  )

