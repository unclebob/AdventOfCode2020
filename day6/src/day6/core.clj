(ns day6.core
  (:require [clojure.string :as string]))


(defn parse-person [person]
  (set person)
  )

(defn count-group [group]
  (let [people (map parse-person group)
        answers (reduce clojure.set/union people)]
    (count answers)))

(defn count-same-answer [group]
  (let [people (map parse-person group)
        answers (reduce clojure.set/intersection people)]
    (count answers)))

(defn count-all-answers [groups]
  (reduce + (map count-group groups)))

(defn count-same-answers [groups]
  (reduce + (map count-same-answer groups)))

(defn parse-groups [groups]
  (map string/trim
       (string/split groups #"\n\n")))

(defn parse-group [group]
  (string/split group #"\n"))



