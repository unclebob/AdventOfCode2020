(ns day2.core)

(defn parse-pw [pw-string]
  (let [[_ min-text max-text letter pw] (re-matches #"(\d+)-(\d+) (\w): (\w+)" pw-string)
        min (Integer/parseInt min-text)
        max (Integer/parseInt max-text)]
    [min max letter pw]))

(defn match-pw [[min max letter pw]]
  (let [letter (first letter)
        letters (filter #(= letter %) pw)
        letter-count (count letters)]
    (and (>= letter-count min)
         (>= max letter-count))))

(defn match-pw2 [[p1 p2 letter pw]]
  (let [letter (first letter)
        p1 (nth pw (dec p1))
        p2 (nth pw (dec p2))
        ls (filter #(= letter %) [p1 p2])]
    (= 1 (count ls))))


(defn solve []
  (let [input (clojure.string/split (slurp "input") #"\n")
        allpws (map parse-pw input)
        goodpws (filter match-pw allpws)]
    (count goodpws)))

(defn solve2 []
  (let [input (clojure.string/split (slurp "input") #"\n")
        allpws (map parse-pw input)
        goodpws (filter match-pw2 allpws)]
    (count goodpws)))