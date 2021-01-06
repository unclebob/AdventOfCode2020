(ns day2.core-spec
  (:require [speclj.core :refer :all]
            [day2.core :refer :all]))

(describe "matcher"
  (it "will will parse the lines"
    (should= [7 10 "j" "pw"] (parse-pw "7-10 j: pw")))
  (it "will match the password"
    (should (match-pw [1 1 "a" "a"]))
    (should-not (match-pw [3 5 "b" "abc"]))
    (should (match-pw [3 5 "b" "abbbc"]))
    (should-not (match-pw [3 5 "b" "abbbbbbc"]))
    ))

(describe "solution"
  (should= nil (solve2)))

