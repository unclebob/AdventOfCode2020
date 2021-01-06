(ns day12.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [day12.data :as data]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)

  {:waypoint [10 1]
   :heading :east
   :position [0 0]
   :trail []
   :commands (data/parse-input (slurp "input"))}
  )

(defn update-state [{:keys
                     [heading position trail commands]
                     :as world}]
  (let [trail (conj trail position)
        command (first commands)
        commands (rest commands)
        world (if (nil? command) world
                                 (data/execute-command-without-waypoint world command))]
    (assoc world :commands commands :trail trail))
  )

(defn draw-state [{:keys
                   [heading position trail]}]
  (q/background 200 220 255)
  (q/translate 500 500)

  (q/fill 0)
  (q/ellipse (/ (first position) 5)
             (/ (second position) 5)
             5 5)
  )

(defn ^:export -main [& args]
  (q/defsketch day12
               :title "Advent of Code: Day 12"
               :size [1000 1000]
               :setup setup
               :update update-state
               :draw draw-state
               :middleware [m/fun-mode])
  args)
