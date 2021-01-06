(ns day12.core-spec
  (:require [speclj.core :refer :all]
            [day12.data :refer :all]))

(defn should-move-ship [world direction distance expected-position]
  (let [world (move-ship world direction distance)
        position (:position world)]
    (should= expected-position position)))

(defn should-go-forward [world distance, expected-position]
  (let [world (forward-ship world distance)
        position (:position world)]
    (should= expected-position position)))

(defn should-move-waypoint [world direction distance expected-position]
  (let [world (move-waypoint world direction distance)
        waypoint (:waypoint world)]
    (should= expected-position waypoint)))

(defn should-move-to-waypoint [world starting distance expected-position]
  (let [world (assoc world :position starting)
        world (forward-to-waypoint world distance)
        position (:position world)]
    (should= expected-position position)))

(defn should-rotate-ship [world direction
                          expected-heading]
  (let [world (rotate-ship world direction)
        heading (:heading world)]
    (should= expected-heading heading)))

(defn should-rotate-waypoint [world direction expected-position]
  (let [world (rotate-waypoint world direction)
        waypoint (:waypoint world)]
    (should= expected-position waypoint)))

(let [world {:heading :east
             :position [0 0]
             :waypoint [10 1]
             :trail []}]
  (describe "moving the ship"
    (it "should go north"
      (should-move-ship world :north 10 [0 10]))

    (it "should go east"
      (should-move-ship world :east 10 [10 0]))

    (it "should go south"
      (should-move-ship world :south 10 [0 -10]))

    (it "should go west"
      (should-move-ship world :west 10 [-10 0]))

    (it "should go forward"
      (should-go-forward world 10 [10 0]))

    (it "should move waypoint east"
      (should-move-waypoint world :east 10 [20 1]))

    (it "should move waypoint west"
      (should-move-waypoint world :west 10 [0 1]))

    (it "should move waypoint north"
      (should-move-waypoint world :north 10 [10 11]))

    (it "should move waypoint south"
      (should-move-waypoint world :south 10 [10 -9]))

    (it "should move to waypoint"
      (should-move-to-waypoint world [1 1] 10 [101 11]))
    )

  (describe "rotating ship"
    (it "should rotate right 90"
      (should-rotate-ship world :right-90 :south))

    (it "should rotate right 180"
      (should-rotate-ship world :right-180 :west))

    (it "should rotate left 90"
      (should-rotate-ship world :left-90 :north))

    (it "should rotate left 180"
      (should-rotate-ship world :left-180 :west))
    )

  (describe "rotate waypoint"
    (it "should rotate waypoint right 90"
      (should-rotate-waypoint world :right-90 [1 -10]))

    (it "should rotate waypoint right 180"
      (should-rotate-waypoint world :right-180 [-10 -1]))

    (it "should rotate waypoint right 270"
      (should-rotate-waypoint world :right-270 [-1 10]))

    (it "should rotate waypoint left 90"
      (should-rotate-waypoint world :left-90 [-1 10]))

    (it "should rotate waypoint left 180"
      (should-rotate-waypoint world :left-180 [-10 -1]))

    (it "should rotate waypoint left 270"
      (should-rotate-waypoint world :left-270 [1 -10]))
    )

  (describe "execute command without waypoints"
    (with-stubs)
    (it "will call move-ship to the north"
      (with-redefs [move-ship (stub :move-ship)]
        (execute-command-without-waypoint world [:move-north 10])
        (should-have-invoked :move-ship
                             {:with [world :north 10]})))

    (it "will call move-ship to the south"
      (with-redefs [move-ship (stub :move-ship)]
        (execute-command-without-waypoint world [:move-south 10])
        (should-have-invoked :move-ship
                             {:with [world :south 10]})))

    (it "will call move-ship to the east"
      (with-redefs [move-ship (stub :move-ship)]
        (execute-command-without-waypoint world [:move-east 10])
        (should-have-invoked :move-ship
                             {:with [world :east 10]})))

    (it "will call move-ship to the west"
      (with-redefs [move-ship (stub :move-ship)]
        (execute-command-without-waypoint world [:move-west 10])
        (should-have-invoked :move-ship
                             {:with [world :west 10]})))

    (it "will call forward-ship "
      (with-redefs [forward-ship (stub :forward-ship)]
        (execute-command-without-waypoint world [:move-forward 10])
        (should-have-invoked :forward-ship
                             {:with [world 10]})))

    (it "will call rotate-ship to the right 90"
      (with-redefs [rotate-ship (stub :rotate-ship)]
        (execute-command-without-waypoint world [:right 90])
        (should-have-invoked :rotate-ship
                             {:with [world :right-90]})))

    (it "will call rotate-ship to the right 180"
      (with-redefs [rotate-ship (stub :rotate-ship)]
        (execute-command-without-waypoint world [:right 180])
        (should-have-invoked :rotate-ship
                             {:with [world :right-180]})))

    (it "will call rotate-ship to the right 270"
      (with-redefs [rotate-ship (stub :rotate-ship)]
        (execute-command-without-waypoint world [:right 270])
        (should-have-invoked :rotate-ship
                             {:with [world :right-270]})))

    (it "will call rotate-ship to the left 90"
      (with-redefs [rotate-ship (stub :rotate-ship)]
        (execute-command-without-waypoint world [:left 90])
        (should-have-invoked :rotate-ship
                             {:with [world :left-90]})))

    (it "will call rotate-ship to the left 180"
      (with-redefs [rotate-ship (stub :rotate-ship)]
        (execute-command-without-waypoint world [:left 180])
        (should-have-invoked :rotate-ship
                             {:with [world :left-180]})))

    (it "will call rotate-ship to the left 270"
      (with-redefs [rotate-ship (stub :rotate-ship)]
        (execute-command-without-waypoint world [:left 270])
        (should-have-invoked :rotate-ship
                             {:with [world :left-270]})))
    )

  (describe "execute with waypoints command"
    (with-stubs)
    (it "will call move-waypoint to the north"
      (with-redefs [move-waypoint (stub :move-waypoint)]
        (execute-command-for-waypoint world [:move-north 10])
        (should-have-invoked :move-waypoint
                             {:with [world :north 10]})))

    (it "will call move-waypoint to the south"
      (with-redefs [move-waypoint (stub :move-waypoint)]
        (execute-command-for-waypoint world [:move-south 10])
        (should-have-invoked :move-waypoint
                             {:with [world :south 10]})))

    (it "will call move-waypoint to the east"
      (with-redefs [move-waypoint (stub :move-waypoint)]
        (execute-command-for-waypoint world [:move-east 10])
        (should-have-invoked :move-waypoint
                             {:with [world :east 10]})))

    (it "will call move-waypoint to the west"
      (with-redefs [move-waypoint (stub :move-waypoint)]
        (execute-command-for-waypoint world [:move-west 10])
        (should-have-invoked :move-waypoint
                             {:with [world :west 10]})))

    (it "will call forward-to-waypoint "
      (with-redefs [forward-to-waypoint (stub :forward-to-waypoint)]
        (execute-command-for-waypoint world [:move-forward 10])
        (should-have-invoked :forward-to-waypoint
                             {:with [world 10]})))

    (it "will call rotate-waypoint to the right 90"
      (with-redefs [rotate-waypoint (stub :rotate-waypoint)]
        (execute-command-for-waypoint world [:right 90])
        (should-have-invoked :rotate-waypoint
                             {:with [world :right-90]})))

    (it "will call rotate-waypoint to the right 180"
      (with-redefs [rotate-waypoint (stub :rotate-waypoint)]
        (execute-command-for-waypoint world [:right 180])
        (should-have-invoked :rotate-waypoint
                             {:with [world :right-180]})))

    (it "will call rotate-waypoint to the right 270"
      (with-redefs [rotate-waypoint (stub :rotate-waypoint)]
        (execute-command-for-waypoint world [:right 270])
        (should-have-invoked :rotate-waypoint
                             {:with [world :right-270]})))

    (it "will call rotate-waypoint to the left 90"
      (with-redefs [rotate-waypoint (stub :rotate-waypoint)]
        (execute-command-for-waypoint world [:left 90])
        (should-have-invoked :rotate-waypoint
                             {:with [world :left-90]})))

    (it "will call rotate-waypoint to the left 180"
      (with-redefs [rotate-waypoint (stub :rotate-waypoint)]
        (execute-command-for-waypoint world [:left 180])
        (should-have-invoked :rotate-waypoint
                             {:with [world :left-180]})))

    (it "will call rotate-waypoint to the left 270"
      (with-redefs [rotate-waypoint (stub :rotate-waypoint)]
        (execute-command-for-waypoint world [:left 270])
        (should-have-invoked :rotate-waypoint
                             {:with [world :left-270]})))
    )
  )

(def test-data "F10\nN3\nF7\nR90\nF11")

(describe "parsing"
  (it "should parse a bunch of commands"
    (should= [[:move-forward 10]
              [:move-north 3]
              [:move-forward 7]
              [:right 90]
              [:move-forward 11]]
             (parse-input test-data))))

(describe "Acceptance Tests"
  (it "solves first acceptance test"
    (let [world {:heading :east
                 :position [0 0]}
          commands (parse-input test-data)
          world (execute-commands-without-waypoint world commands)
          position (:position world)]
      (should= 25 (manhattan-distance position))))

  (it "solves second acceptance test"
    (let [world {:position [0 0]
                 :waypoint [10 1]
                 :trail []}
          commands (parse-input test-data)
          world (execute-commands-with-waypoint world commands)
          position (:position world)]
      (should= 286 (manhattan-distance position))))
  )

(describe "solutions"
  (it "solves first part"
    (let [world {:heading :east
                 :position [0 0]
                 :trail []}
          commands (parse-input (slurp "input"))
          world (execute-commands-without-waypoint world commands)
          position (:position world)]
      (should= 1106 (manhattan-distance position))))

  (it "solves first part"
    (let [world {:position [0 0]
                 :waypoint [10 1]
                 :trail []}
          commands (parse-input (slurp "input"))
          world (execute-commands-with-waypoint world commands)
          position (:position world)]
      (should= 107281 (manhattan-distance position))))
  )
