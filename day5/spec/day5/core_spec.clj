(ns day5.core-spec
  (:require [speclj.core :refer :all]
            [day5.core :refer :all]))

(describe "decode seat id"
  (it "decodes"
    (should= 127 (seat-id "BBBBBBB"))
    (should= 81 (seat-id "BLBLFLR"))
    (should= 357 (seat-id "FBFBBFFRLR"))))

(describe "solutions"
  (it "solved#1"
    (should= 922 (solve-1)))
  (it "solved#2"
    (should= 747 (solve-2))
    ))