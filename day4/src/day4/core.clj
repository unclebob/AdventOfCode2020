(ns day4.core
  (:require [clojure.string :as string]))

(def valid-with-cid (sort ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"]))
(def valid-no-cid (sort ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn get-passports [input]
  (string/split input #"\n\n"))

(defn parse-passport [passport]
  (let [fields (string/split passport #"\s")
        pairs (map #(rest (re-matches #"(\w{3}):(.*)" %)) fields)]
    (apply hash-map (flatten pairs)))
  )

(defn valid-hgt? [value]
  (let [[val unit] (rest (re-matches #"(\d+)([a-z]{2}).*" value))]
    (condp = unit
      "cm" (<= 150 (Integer/parseInt val) 193)
      "in" (<= 59 (Integer/parseInt val) 76)
      false)))

(defn valid-field? [[key value]]
  (condp = key
    "byr" (<= 1920 (Integer/parseInt value) 2002)
    "iyr" (<= 2010 (Integer/parseInt value) 2020)
    "eyr" (<= 2020 (Integer/parseInt value) 2030)
    "hgt" (valid-hgt? value)
    "hcl" (re-matches #"#[0-9a-f]{6}.*" value)
    "ecl" (re-matches #"amb|blu|brn|gry|grn|hzl|oth" value)
    "pid" (re-matches #"\d{9}" value)
    "cid" true
    false
    ))

(defn valid-passport? [passport]
  (let [passport-keys (sort (keys passport))
        keys-correct? (or (= valid-with-cid passport-keys)
                          (= valid-no-cid passport-keys))]
    (if (not keys-correct?)
      false
      (let [field-validations (map valid-field? passport)]
        (reduce #(and %1 %2) field-validations)))
    )
  )

(defn solve []
  (let [input (slurp "input")
        raw-passports (get-passports input)
        passports (map parse-passport raw-passports)
        valid-passports (filter valid-passport? passports)]
    (count valid-passports)
    )
  )

