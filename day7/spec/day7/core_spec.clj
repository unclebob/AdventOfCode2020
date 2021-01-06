(ns day7.core-spec
  (:require [speclj.core :refer :all]
            [day7.core :refer :all]))

(def test-input "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")
(def test-input-2 "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.")

(describe "parsing"
  (it "parses a rule"
    (should= ["key" [[1 "v1"] [2 "v2"]]]
             (parse-rule "key bags contain 1 v1 bag, 2 v2 bags"))
    (should= ["key" []]
             (parse-rule "key bags contain no other bags."))
    )

  (it "parses the input text"
    (let [rules (parse-rules test-input)]
      (should= [[2 "shiny gold"] [9 "faded blue"]]
               (rules "muted yellow"))
      (should= [] (rules "dotted black")))
    )
  )

(describe "acceptance tests"
  (it "counts bags that can contain shiny-gold"
    (should= 4 (count-containers
                 "shiny gold"
                 (parse-rules test-input))))

  (it "counts bags that shiny gold contains"
    (should= 32 (count-contained-bags
                  "shiny gold"
                  (parse-rules test-input)))

    (should= 126 (count-contained-bags
                   "shiny gold"
                   (parse-rules test-input-2))))
  )

(describe "setting up solution 1"
  (it "can remove counts from rules"
    (should= ["container" #{"contained"}] (remove-numbers ["container" #{[5 "contained"]}]))))

(describe "solutions"
  (it "solves first + second problem"
    (let [input (slurp "input")
          rules (parse-rules input)]
      (should= 378 (count-containers "shiny gold" rules))
      (should= 0 (count-contained-bags "shiny gold" rules))))
  )


