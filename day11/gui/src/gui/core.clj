(ns gui.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [gui.data :as data]))

(defn setup []
  (q/frame-rate 10)
  (q/color-mode :rgb)
  {:world data/real-data}
)

(defn update-state [{:keys [world] :as state}]
  (assoc state :world (data/update-automaton-2 world))
 )

(defn draw-empty-chair [r c]
  (q/fill 255)
  (q/stroke 0)
  (q/ellipse r c 8 8)
  )

(defn draw-floor [r c]

  )

(defn draw-person [r c]
  (q/fill 255 0 0)
  (q/ellipse r c 8 8)
  )

(defn draw-state [{:keys [world]}]
  (q/translate 10 10)
  (q/background 255)
  (doseq [r (range (count world)) c (range (count (first world)))]
    (let [y (* r 10)
          x (* c 10)]
      (condp = (data/get-cell world r c)
        :empty (draw-empty-chair x y)
        :person (draw-person x y)
        :floor (draw-floor x y)))
    )
  )


(defn ^:export -main [& args]
  (q/defsketch gui
               :title "Advent of Code Day 11 Cellular Automaton"
               :size [1000 1000]
               :setup setup
               :update update-state
               :draw draw-state
               :middleware [m/fun-mode])
  args)
