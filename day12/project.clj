(defproject gui "0.1.0-SNAPSHOT"
  :description "Advent of Code, Day 12"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]]
  :main day12.core
  :profiles {:dev {:dependencies [[speclj "3.3.2"]]}}
    :plugins [[speclj "3.3.2"]]
    :test-paths ["spec"]
  )
