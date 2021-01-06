(ns day6.core-spec
  (:require [speclj.core :refer :all]
            [day6.core :refer :all]))

(describe "can count answers in groups"
  (it "counts a group"
    (should= 3 (count-group ["ab" "bc"]))
    )

  (it "can count groups"
    (should= 5 (count-all-answers [
                              [#{\a \b} #{\b \c}]
                              [#{\a \b} #{\b}]
                              ]))
    ))

(describe "parsing"
  (it "parses groups"
    (should= ["p1\np2" "p3\np4"]
             (parse-groups "p1\np2\n\np3\np4\n"))
    )

  (it "parses a group"
    (should= ["ac" "b"] (parse-group "ac\nb")))

  (it "parses a person"
    (should= #{\a \b} (parse-person "ab"))))

(describe "solutions"
  (let [input (slurp "input")
        groups (map parse-group (parse-groups input))]
    (it "solves1"
      (should= 6249 (count-all-answers groups))
      )

    (it "solves2 same answers"
      (should= 0 (count-same-answers groups)))))