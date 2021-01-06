(ns day4.core-spec
  (:require [speclj.core :refer :all]
            [day4.core :refer :all]))

(describe "the parser"
  (it "should separate passports by blank lines"
    (should= ["pp1\nx" "pp2\nx"]
             (get-passports "pp1\nx\n\npp2\nx")))

  (it "should parse a single passport"
    (should= {"bbb" "yyy", "aaa" "xxx"}
             (parse-passport "aaa:xxx bbb:yyy")))

  )

(describe "solution"
  (it "is solved"
    (should= 196 (solve)))
  )
