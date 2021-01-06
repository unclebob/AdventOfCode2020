(ns day8.core-spec
  (:require [speclj.core :refer :all]
            [day8.core :refer :all]))

(describe "instructions"
  (let [init-machine {:pc 0 :ac 0}]

    (it "should execute a nop"
      (let [machine (nop init-machine)]
        (should= 1 (:pc machine))
        (should= 0 (:ac machine))))

    (it "should execute an acc"
      (let [machine (acc +2 init-machine)]
        (should= 1 (:pc machine))
        (should= 2 (:ac machine))))

    (it "should execute a jmp"
      (let [machine (jmp -3 init-machine)]
        (should= 0 (:ac machine))
        (should= -3 (:pc machine))))))

(describe "execution"
  (let [program [[:acc 1] [:jmp 2] [:acc -1] [:nop 0]]]
    (it "should execute"
      (let [{:keys [ac pc history]} (execute program)]
        (should= 4 pc)
        (should= 1 ac)
        (should= [0 1 3] history)))

    (it "should halt when asked to repeat an instruction"
      (let [program [[:acc 1] [:jmp -1] [:nop 0]]
            machine (execute program)]
        (should= 0 (:pc machine))
        (should= 1 (:ac machine))))))

(describe "parsing"
  (it "should parse one instruction"
    (should= [:jmp -1] (parse-instruction "jmp -1")))

  (it "should parse many instuctions"
    (should= [[:jmp 1] [:nop 4]]
             (parse-instructions "jmp +1\nnop +4\n"))))

(describe "Acceptance tests"
  (let [input "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"
        program (parse-instructions input)]
    (it "should pass first acceptance test"
      (should= 5 (:ac (execute program))))

    (it "should pass second acceptance test"
      (should= 8 (fix-program program 0)))))

(describe "Solutions"
  (it "Solves first problem"
    (let [input (slurp "input")
          program (parse-instructions input)
          machine (execute program)]
      (should= 1810 (:ac machine))))

  (it "fixes broken program"
    (let [input (slurp "input")
          program (parse-instructions input)]
      (should= nil (fix-program program 0)))
    )
  )

