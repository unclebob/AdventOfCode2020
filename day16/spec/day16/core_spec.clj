(ns day16.core-spec
  (:require [speclj.core :refer :all]
            [day16.core :refer :all]
            [clojure.string :as string]))

(defn to-int [i]
  (Integer/parseInt i))

(defn parse-line [line]
  (if-let [[_ name a b c d] (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+).*" line)]
    [:field name [(to-int a) (to-int b)] [(to-int c) (to-int d)]]
    (if (re-matches #"[,\d]+" line)
      (let [values (string/split line, #",")]
        (into [:ticket] (mapv #(Integer/parseInt %) values)))
      nil)))

(defn parse-input [input]
  (let [lines (string/split-lines input)
        lines (map parse-line lines)
        fields (map rest (filter #(= :field (first %)) lines))
        tickets (map rest (filter #(= :ticket (first %)) lines))
        ]
    {:fields fields :ticket (first tickets) :nearby (rest tickets)}))

(def test-data-1 (str "class: 1-3 or 5-7\n"
                      "row: 6-11 or 33-44\n"
                      "seat: 13-40 or 45-50\n"
                      "\n"
                      "your ticket:\n"
                      "7,1,14\n"
                      "\n"
                      "nearby tickets:\n"
                      "7,3,47\n"
                      "40,4,50\n"
                      "55,2,20\n"
                      "38,6,12\n"))

(def test-data-2 (str "class: 0-1 or 4-19\n"
                      "row: 0-5 or 8-19\n"
                      "seat: 0-13 or 16-19\n"
                      "\n"
                      "your ticket:\n"
                      "11,12,13\n"
                      "\n"
                      "nearby tickets:\n"
                      "3,9,18\n"
                      "15,1,5\n"
                      "5,14,9\n"))

(defn valid-for-field? [n [_ [a b] [c d]]]
  (or (<= a n b) (<= c n d)))

(defn valid-for-some-field? [n fields]
  (some identity (map #(valid-for-field? n %) fields)))

(defn valid-ticket? [ticket fields]
  (let [checks (map #(valid-for-some-field? % fields) ticket)]
    (every? identity checks)))

(defn gather-ticket-values [{:keys [nearby]}]
  (loop [ticket-values []
         tickets nearby]
    (if (empty? tickets)
      ticket-values
      (recur (into ticket-values (first tickets))
             (rest tickets)))))

(defn find-invalid-numbers [{:keys [nearby fields] :as input}]
  (let [ticket-values (gather-ticket-values input)]
    (loop [invalid []
           values ticket-values]
      (if (empty? values)
        invalid
        (if (valid-for-some-field? (first values) fields)
          (recur invalid (rest values))
          (recur (conj invalid (first values)) (rest values)))))))

(defn solve-1 [input]
  (let [invalid-numbers (find-invalid-numbers input)]
    (reduce + invalid-numbers)))

(defn find-valid-tickets [{:keys [fields nearby]}]
  (filter #(valid-ticket? % fields) nearby))

(defn build-field-filter [{:keys [fields ticket nearby]}]
  (let [field-count (count ticket)
        ordinals (set (range field-count))]
    (loop [filter {}
           fields fields]
      (if (empty? fields)
        filter
        (let [[name _] (first fields)]
          (recur (assoc filter name ordinals) (rest fields)))))))

(defn remove-ordinal [filter name ordinal]
  (update filter name #(disj % ordinal)))

(defn filter-ticket [filter field ticket]
  (let [name (first field)]
    (loop [ordinal 0
           ticket ticket
           filter filter]
      (if (empty? ticket)
        filter
        (if (valid-for-field? (first ticket) field)
          (recur (inc ordinal)
                 (rest ticket)
                 filter)
          (recur (inc ordinal)
                 (rest ticket)
                 (remove-ordinal filter name ordinal)))))))

(defn filter-field [filter field tickets]
  (loop [tickets tickets
         filter filter]
    (if (empty? tickets)
      filter
      (recur (rest tickets)
             (filter-ticket filter field (first tickets))))))

(defn resolve-field [field-filter unresolved-key resolved-keys]
  (loop [field-filter field-filter
         resolved-keys resolved-keys]
    (if (empty? resolved-keys)
      field-filter
      (let [resolved-ordinal (first (field-filter (first resolved-keys)))]
        (recur (update field-filter unresolved-key #(disj % resolved-ordinal))
               (rest resolved-keys))))))

(defn resolve-filter [field-filter]
  (let [resolved-keys (filter #(= 1 (count (field-filter %))) (keys field-filter))
        unresolved-keys (remove #(= 1 (count (field-filter %))) (keys field-filter))]
    (if (empty? unresolved-keys)
      field-filter
      (loop [unresolved unresolved-keys
             field-filter field-filter]
        (assert (pos? (count resolved-keys)))
        (if (empty? unresolved)
          (resolve-filter field-filter)
          (recur (rest unresolved)
                 (resolve-field field-filter (first unresolved) resolved-keys)))))))

(defn determine-field-ordinals [{:keys [fields nearby] :as input}]
  (let [filter (build-field-filter input)
        tickets (find-valid-tickets input)]
    (loop [fields fields
           filter filter
           tickets tickets]
      (if (empty? fields)
        (resolve-filter filter)
        (recur (rest fields)
               (filter-field filter (first fields) tickets)
               tickets)))))

(defn get-ticket-field [ticket key field-ordinals]
  (let [ordinal (first (field-ordinals key))]
    (nth ticket ordinal)))

(defn solve-2 [{:keys [ticket] :as input}]
  (let [field-ordinals (determine-field-ordinals input)
        departure-keys (filter #(string/starts-with? % "departure") (keys field-ordinals))
        ticket-fields (map #(get-ticket-field ticket % field-ordinals) departure-keys)]
    (reduce * ticket-fields)))

(describe "Day 16, Validating the Tickets"
  (context "Part 1: find the scanning error rate"
    (context "parsing"
      (it "should parse a field description"
        (should= [:field "name" [1 3] [5 17]] (parse-line "name: 1-3 or 5-17")))

      (it "will parse a ticket."
        (should= [:ticket 1 2 3 4 5] (parse-line "1,2,3,4,5"))
        (should= [:ticket 1] (parse-line "1")))

      (it "will not parse junk"
        (should-be-nil (parse-line "junk")))

      (it "will parse input"
        (should= {:fields [["field" [1 2] [5 6]]]
                  :ticket [1]
                  :nearby [[2] [3]]}
                 (parse-input "field: 1-2 or 5-6\n1\n2\n3")))

      (it "will parse test-data"
        (should= {:fields [["class" [1 3] [5 7]]
                           ["row" [6 11] [33 44]]
                           ["seat" [13 40] [45 50]]]
                  :ticket [7 1 14]
                  :nearby [[7 3 47]
                           [40 4 50]
                           [55 2 20]
                           [38 6 12]]}
                 (parse-input test-data-1))))

    (context "find invalid ticket numbers"
      (it "will determine validity of a number for one field"
        (should (valid-for-field? 6 ["f1" [1 7] [12 15]]))
        (should (valid-for-field? 1 ["f1" [1 7] [12 15]]))
        (should (valid-for-field? 7 ["f1" [1 7] [12 15]]))
        (should (valid-for-field? 12 ["f1" [1 7] [12 15]]))
        (should (valid-for-field? 13 ["f1" [1 7] [12 15]]))
        (should (valid-for-field? 15 ["f1" [1 7] [12 15]]))

        (should-not (valid-for-field? 8 ["f1" [1 7] [12 15]]))
        (should-not (valid-for-field? 9 ["f1" [1 7] [12 15]]))
        (should-not (valid-for-field? 11 ["f1" [1 7] [12 15]]))
        (should-not (valid-for-field? 16 ["f1" [1 7] [12 15]])))

      (it "will determine validity of a ticket"
        (let [fields [["f1" [1 2] [4 5]]
                      ["f2" [7 8] [12 15]]]]
          (should (valid-ticket? [8 2] fields))
          (should (valid-ticket? [13 8] fields))

          (should-not (valid-ticket? [1 3] fields))
          (should-not (valid-ticket? [1 16] fields))
          (should-not (valid-ticket? [6 3] fields))))

      (it "will gather up all the ticket values"
        (should= (sort [7 3 47 40 4 50 55 2 20 38 6 12])
                 (sort (gather-ticket-values (parse-input test-data-1)))))

      (it "will find the ticket values that fail the validations"
        (should= (sort [4 55 12]) (sort (find-invalid-numbers (parse-input test-data-1))))))

    (context "Acceptance Test"
      (it "will pass test 1"
        (should= 71 (solve-1 (parse-input test-data-1)))))

    (context "solve problem 1"
      (let [input (parse-input (slurp "input"))]
        (it "should solve problem 1"
          (should= 28884 (solve-1 input))))))

  (context "Part 2, find the departure values"
    (it "find valid tickets"
      (should= [[1] [2] [3]] (find-valid-tickets {:fields [["f1" [1 3] [10 12]]]
                                                  :ticket [1]
                                                  :nearby [[1] [7] [2] [8] [3] [99]]})))

    (it "should build field filter"
      (should= {"class" #{0 1 2}
                "row" #{0 1 2}
                "seat" #{0 1 2}} (build-field-filter (parse-input test-data-2))))

    (it "should resolve a field"
      (should= {"f1" #{1}
                "f2" #{2}} (resolve-field {"f1" #{1}
                                           "f2" #{1 2}} "f2" ["f1"])))

    (it "resolves a filter"
      (should= {"f1" #{1}
                "f2" #{2}} (resolve-filter {"f1" #{1 2}
                                            "f2" #{2}})))

    (it "should remove invalid ordinals from the filter"
      (should= {"class" #{1}
                "row" #{0}
                "seat" #{2}} (determine-field-ordinals (parse-input test-data-2))))
    (it "should solve"
      (should= 1001849322119 (solve-2 (parse-input (slurp "input")))))
    )
  )
