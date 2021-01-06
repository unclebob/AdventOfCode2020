(ns day7.core
  (:require [clojure.string :as string]))

(defn format-bag [[_ number name]]
  (if (nil? number)
    []
    [(Integer/parseInt number) name]))

(defn parse-rule [rule]
  (let [[_ key predicate]
        (re-matches #"(.+) bags contain (.+)" rule)
        bag-texts (clojure.string/split predicate #",")
        bag-texts (map clojure.string/trim bag-texts)
        bags (map #(re-matches #"(\d+) (.+) bag.*" %) bag-texts)
        bags (remove nil? bags)
        bags (map format-bag bags)]
    [key bags]))

(defn parse-rules [input]
  (let [rule-lines (string/split input #"\n")
        rules (map parse-rule rule-lines)
        rules (into (hash-map) rules)]
    rules))

(defn invert-map-of-sets [m]
  (reduce (fn [a [k v]] (assoc a k (conj (get a k #{}) v)))
          {}
          (for [[k s] m v s] [v k])))

(defn remove-number [[number bag]]
  bag)

(defn remove-numbers [[bag rule]]
  [bag (set (map remove-number rule))]
  )

(defn do-count-containers [bags bag tree]
  (if (zero? (count (tree bag)))
    bags
    (let [paths (for [child (tree bag)]
                  (do-count-containers (conj bags child) child tree))]
      (reduce clojure.set/union paths)))
  )

(defn count-containers [bag rules]
  (let [rules (map remove-numbers rules)
        tree (invert-map-of-sets rules)]
    (count (do-count-containers #{} bag tree))
    ))

(defn do-count-contained-bags [bag rules]
  (let [children (rules bag)]
    (if (zero? (count children))
      1
      (let [answer (+ 1
                      (reduce +
                              (for [[n child] children]
                                (* n (do-count-contained-bags child rules)))))]
        answer))))

(defn count-contained-bags [bag rules]
  (dec (do-count-contained-bags bag rules)))
