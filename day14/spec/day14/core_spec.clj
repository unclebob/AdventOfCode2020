(ns day14.core-spec
  (:require [speclj.core :refer :all]
            [day14.core :refer :all]
            [clojure.string :as string]))



(defn to-binary [n]
  (Long/toBinaryString n)
  )

(defn to-decimal [bs]
  (Long/parseLong bs 2)
  )

(defn mask-bits [mask bs]
  (let [r-bs (reverse bs)
        r-mask (reverse mask)
        max-bits (max (count r-bs) (count r-mask))
        r-bs (vec (concat r-bs (repeat 36 \0)))
        r-mask (vec (concat r-mask (repeat 36 \X)))
        masked-bits (for [n (range max-bits)]
                      (condp = (get r-mask n)
                        \X (get r-bs n)
                        \0 \0
                        \1 \1))]
    (apply str (reverse masked-bits)))
  )

(defn parse-line [line]
  (cond
    (string/starts-with? line "mask = ")
    [:mask (subs line 7)]

    (string/starts-with? line "mem[")
    (let [[_ addr value] (re-matches #"mem\[(\d+)\] = (.*)" line)]
      [:mem (Long/parseLong addr) (Long/parseLong value)]))
  )

(defn solve-1 [input]
  (let [lines (string/split-lines input)
        commands (map parse-line lines)]
    (loop [mask nil
           memory {}
           [command & commands] commands]
      (cond
        (nil? command)
        (reduce + (vals memory))

        (= :mask (first command))
        (recur (second command) memory commands)

        (= :mem (first command))
        (let [value (nth command 2)
              addr (second command)
              result (to-decimal (mask-bits mask (to-binary value)))]
          (recur mask (assoc memory addr result) commands))))))

(defn make-addr [pattern f-addr]
  (loop [pattern pattern
         f-addr f-addr
         addr []]
    (if (empty? f-addr)
      (apply str addr)
      (if (= \X (first f-addr))
        (recur (rest pattern) (rest f-addr) (conj addr (first pattern)))
        (recur pattern (rest f-addr) (conj addr (first f-addr)))))))

(defn make-addrs [addr]
  (let [xs (count (filter #(= \X %) addr))]
    (if (zero? xs)
      [addr]
      (let [patterns (map #(Long/toBinaryString %) (range 0 (Math/pow 2 xs)))
            patterns (map #(concat (repeat xs \0) %) patterns)
            patterns (map #(reverse (subvec (vec (reverse %)) 0 xs)) patterns)]
        (loop [[pattern & patterns] patterns
               results []]
          (if (nil? pattern)
            results
            (recur patterns (conj results (make-addr pattern addr)))))))))

(defn mask-addr [mask bits]
  (let [size (max (count mask) (count bits))
        pad (repeat size \0)
        mask (vec (concat pad mask))
        bits (vec (concat pad bits))
        mask (vec (reverse (subvec (vec (reverse mask)) 0 size)))
        bits (vec (reverse (subvec (vec (reverse bits)) 0 size)))
        indices (range size)
        result (for [i indices]
                 (condp = (get mask i)
                   \X \X
                   \1 \1
                   \0 (get bits i)))]
    (apply str result)))

(defn store-vals [mask addr val memory]
  (let [addrs (make-addrs (mask-addr mask addr))]
    (loop [[addr & addrs] addrs
           memory memory]
      (if (nil? addr)
        memory
        (recur addrs (assoc memory (to-decimal addr) val))))))

(defn solve-2 [input]
  (let [lines (string/split-lines input)
        commands (map parse-line lines)]
    (loop [mask nil
           memory {}
           [command & commands] commands]
      (cond
        (nil? command)
        (reduce + (vals memory))

        (= :mask (first command))
        (recur (second command) memory commands)

        (= :mem (first command))
        (let [addr (to-binary (second command))
              value (nth command 2)]
          (recur mask
                 (store-vals mask addr value memory)
                 commands))))))

(describe "day 14"
  (context "part 1"
    (context "utilities"
      (it "should turn numbers into binary strings"
        (should= "101" (to-binary 5)))

      (it "should turn binary strings into numbers"
        (should= 5 (to-decimal "101")))

      (it "should mask a binary string"
        (should= "1101" (mask-bits "XXX" "1101"))
        (should= "001101" (mask-bits "XXXXXX" "1101"))
        (should= "111000" (mask-bits "111000" "000111"))
        (should= "11100" (mask-bits "X1X0X" "10110")))
      )

    (context "parsing"
      (it "should parse a mask line"
        (should= [:mask "XXX"] (parse-line "mask = XXX")))
      (it "should parse a mem line"
        (should= [:mem 8 1011] (parse-line "mem[8] = 1011")))
      )

    (context "Acceptance Test"
      (let [test-data (str "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n"
                           "mem[8] = 11\n"
                           "mem[7] = 101\n"
                           "mem[8] = 0")]
        (it "should pass acceptance test"
          (should= 165 (solve-1 test-data))))
      )

    (it "solves part 1"
      (should= 9879607673316 (solve-1 (slurp "input"))))
    )

  (context "Part 2, the floating bits"
    (it "floats bits"
      (should= ["0000"] (make-addrs "0000"))
      (should= ["0000" "0001"] (make-addrs "000X"))
      (should= ["1000" "1001" "1100" "1101"] (make-addrs "1X0X"))
      )

    (it "masks bits"
      (should= "1010" (mask-addr "0000" "1010"))
      (should= "1010" (mask-addr "1010" "0000"))
      (should= "X11X" (mask-addr "X10X" "1111"))
      )

    (it "passes acceptance test"
      (let [test-data (str "mask = 000000000000000000000000000000X1001X\n"
                           "mem[42] = 100\n"
                           "mask = 00000000000000000000000000000000X0XX\n"
                           "mem[26] = 1")]
        (should= 208 (solve-2 test-data))))

    (it "solves part 2"
      (let [input (slurp "input")]
        (should= 3435342392262 (solve-2 input))))
    )
  )
