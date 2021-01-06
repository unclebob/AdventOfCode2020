(ns day12.data)

(defn move-point [world direction distance point]
  (let [[dx dy] (condp = direction
                  :east [1 0]
                  :west [-1 0]
                  :north [0 1]
                  :south [0 -1])
        [x y] (point world)
        new-position [(+ x (* distance dx))
                      (+ y (* distance dy))]]
    (assoc world point new-position)))


(defn move-ship [world direction distance]
  (move-point world direction distance :position))

(defn move-waypoint [world direction distance]
  (move-point world direction distance :waypoint))

(defn forward-ship [world distance]
  (move-ship world (:heading world) distance))

(defn v-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn v-scale [m [x y]]
  [(* m x) (* m y)])

(defn forward-to-waypoint [world distance]
  (let [position (:position world)
        waypoint (:waypoint world)
        new-position (v-add position (v-scale distance waypoint))]
    (assoc world :position new-position)
    )
  )

(defn- heading->compass [heading]
  (condp = heading
    :east 90
    :south 180
    :west 270
    :north 0))

(defn- get-rotation [direction]
  (condp = direction
    :right-90 90
    :right-180 180
    :right-270 270
    :left-90 -90
    :left-180 -180
    :left-270 -270))

(defn- compass->heading [compass]
  (condp = compass
    0 :north
    90 :east
    180 :south
    270 :west))

(defn rotate-ship [world direction]
  (let [heading (:heading world)
        compass (heading->compass heading)
        rotation (get-rotation direction)
        final-compass (mod (+ compass rotation) 360)]
    (assoc world :heading (compass->heading final-compass))))

(defn v-rotate-right-90 [[x y]]
  [y (- x)])

(defn v-rotate-left-90 [[x y]]
  [(- y) x])

(defn rotate-waypoint [world direction]
  (let [waypoint (:waypoint world)
        new-waypoint (condp = direction
                       :right-90 (v-rotate-right-90 waypoint)
                       :right-180 (v-rotate-right-90
                                    (v-rotate-right-90 waypoint))
                       :right-270 (v-rotate-right-90
                                    (v-rotate-right-90
                                      (v-rotate-right-90 waypoint)))
                       :left-90 (v-rotate-left-90 waypoint)
                       :left-180 (v-rotate-left-90
                                   (v-rotate-left-90 waypoint))
                       :left-270 (v-rotate-left-90
                                   (v-rotate-left-90
                                     (v-rotate-left-90 waypoint)))
                       )]
    (assoc world :waypoint new-waypoint))
  )

(defn execute-command-without-waypoint [world [command arg]]
  (condp = command
    :move-north (move-ship world :north arg)
    :move-south (move-ship world :south arg)
    :move-east (move-ship world :east arg)
    :move-west (move-ship world :west arg)
    :move-forward (forward-ship world arg)
    :right (rotate-ship world (condp = arg
                                90 :right-90
                                180 :right-180
                                270 :right-270))
    :left (rotate-ship world (condp = arg
                               90 :left-90
                               180 :left-180
                               270 :left-270))
    )
  )

(defn execute-command-for-waypoint [world [command arg]]
  (condp = command
    :move-north (move-waypoint world :north arg)
    :move-south (move-waypoint world :south arg)
    :move-east (move-waypoint world :east arg)
    :move-west (move-waypoint world :west arg)
    :move-forward (forward-to-waypoint world arg)
    :right (rotate-waypoint world (condp = arg
                                    90 :right-90
                                    180 :right-180
                                    270 :right-270))
    :left (rotate-waypoint world (condp = arg
                                   90 :left-90
                                   180 :left-180
                                   270 :left-270))
    )
  )

(defn parse-line [line]
  (let [command (first line)
        arg (Integer/parseInt (subs line 1))]
    (condp = command
      \F [:move-forward arg]
      \N [:move-north arg]
      \S [:move-south arg]
      \E [:move-east arg]
      \W [:move-west arg]
      \R [:right arg]
      \L [:left arg]))
  )

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    (mapv parse-line lines)))

(defn execute-commands-without-waypoint [world commands]
  (loop [world world commands commands]
    (if (empty? commands)
      world
      (recur (execute-command-without-waypoint world (first commands))
             (rest commands)))))

(defn execute-commands-with-waypoint [world commands]
  (loop [world world commands commands]
    (if (empty? commands)
      world
      (recur (execute-command-for-waypoint world (first commands))
             (rest commands)))))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

