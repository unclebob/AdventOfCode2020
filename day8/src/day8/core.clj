(ns day8.core)

(defn nop [machine]
  (update machine :pc inc))

(defn acc [arg {:keys [pc ac]}]
  {:ac (+ ac arg) :pc (inc pc)})

(defn jmp [arg machine]
  (update machine :pc + arg))

(defn execute-instruction [machine program]
  (let [[op-code arg] (get program (:pc machine))
        history (:history machine)
        pc (:pc machine)
        new-machine (condp = op-code
                      :nop (nop machine)
                      :jmp (jmp arg machine)
                      :acc (acc arg machine)
                      machine)
        new-machine (assoc new-machine :history (conj history pc))]
    new-machine))

(defn execute [program]
  (loop [machine {:ac 0 :pc 0 :history []}]
    (if (or (>= (:pc machine) (count program))
            (contains? (set (:history machine)) (:pc machine)))
      machine
      (recur (execute-instruction machine program))
      )))

(defn parse-instruction [instruction]
  (let [[m op-code arg] (re-matches #"([a-z]{3}) (.\d+)"
                                    instruction)]
    [(keyword op-code) (Integer/parseInt arg)]))

(defn parse-instructions [instructions]
  (mapv parse-instruction
        (clojure.string/split instructions #"\n")))

(defn patchable? [program fix-loc]
  (let [[op-code _] (get program fix-loc)]
    (or (= op-code :jmp)
        (= op-code :nop))))

(defn patch [program fix-loc]
  (let [[op-code arg] (get program fix-loc)]
    (if (= op-code :jmp)
      (assoc program fix-loc [:nop arg])
      (assoc program fix-loc [:jmp arg]))))

(defn fix-program [program fix-loc]
  (cond
    (= fix-loc (count program))
    nil

    (not (patchable? program fix-loc))
    (fix-program program (inc fix-loc))

    :else
    (let [new-program (patch program fix-loc)
          _ (println new-program)
          {:keys [pc ac]} (execute new-program)]
      (if (= pc (count program))
        ac
        (fix-program program (inc fix-loc))))))