#!/usr/bin/env clojure -M

;; Day 17: Chronospatial Computer - 3-bit VM emulator

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [text]
  "Parse registers and program from input."
  (let [lines (str/split-lines (str/trim text))
        a (Long/parseLong (second (re-find #"Register A: (\d+)" (nth lines 0))))
        b (Long/parseLong (second (re-find #"Register B: (\d+)" (nth lines 1))))
        c (Long/parseLong (second (re-find #"Register C: (\d+)" (nth lines 2))))
        program-str (second (re-find #"Program: ([\d,]+)" (nth lines 4)))
        program (vec (map #(Long/parseLong %) (str/split program-str #",")))]
    {:a a :b b :c c :program program}))

(defn combo-operand [operand a b c]
  "Get combo operand value."
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 a
    5 b
    6 c
    (throw (ex-info "Invalid combo operand" {:operand operand}))))

(defn run-program [a b c program]
  "Execute the 3-bit computer program and return output."
  (let [program-len (count program)]
    (loop [ip 0
           a a
           b b
           c c
           output []]
      (if (>= ip program-len)
        output
        (let [opcode (nth program ip)
              operand (nth program (inc ip))]
          (case opcode
            ;; adv - A = A >> combo
            0 (let [shift (combo-operand operand a b c)]
                (recur (+ ip 2) (bit-shift-right a shift) b c output))

            ;; bxl - B = B XOR literal
            1 (recur (+ ip 2) a (bit-xor b operand) c output)

            ;; bst - B = combo % 8
            2 (let [val (bit-and (combo-operand operand a b c) 7)]
                (recur (+ ip 2) a val c output))

            ;; jnz - jump if A != 0
            3 (if (not= a 0)
                (recur operand a b c output)
                (recur (+ ip 2) a b c output))

            ;; bxc - B = B XOR C (ignores operand)
            4 (recur (+ ip 2) a (bit-xor b c) c output)

            ;; out - output combo % 8
            5 (let [val (bit-and (combo-operand operand a b c) 7)]
                (recur (+ ip 2) a b c (conj output val)))

            ;; bdv - B = A >> combo
            6 (let [shift (combo-operand operand a b c)]
                (recur (+ ip 2) a (bit-shift-right a shift) c output))

            ;; cdv - C = A >> combo
            7 (let [shift (combo-operand operand a b c)]
                (recur (+ ip 2) a b (bit-shift-right a shift) output))))))))

(defn part1 [text]
  "Run the program and return comma-separated output."
  (let [{:keys [a b c program]} (parse-input text)
        output (run-program a b c program)]
    (str/join "," output)))

(defn part2 [text]
  "Find initial A value that makes program output itself."
  (let [{:keys [b c program]} (parse-input text)
        program-len (count program)]

    ;; Work backwards from the last digit - build A 3 bits at a time
    (letfn [(search [target-idx current-a]
              (if (< target-idx 0)
                current-a
                ;; Try all 8 possible 3-bit values for this position
                (loop [bits 0]
                  (if (>= bits 8)
                    nil
                    (let [candidate-a (bit-or (bit-shift-left current-a 3) bits)]
                      ;; A can't be 0 at start (would halt immediately without output)
                      (if (and (= candidate-a 0) (= target-idx (dec program-len)))
                        (recur (inc bits))
                        (let [output (run-program candidate-a b c program)
                              expected (subvec program target-idx)]
                          (if (= output expected)
                            (if-let [result (search (dec target-idx) candidate-a)]
                              result
                              (recur (inc bits)))
                            (recur (inc bits))))))))))]
      (search (dec program-len) 0))))

(defn -main []
  (let [input-path (str (System/getProperty "user.dir") "/../input.txt")
        text (slurp input-path)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
