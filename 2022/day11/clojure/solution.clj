#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-numbers
  "Extract all integers from a string."
  [s]
  (map #(Long/parseLong %) (re-seq #"\d+" s)))

(defn parse-monkey
  "Parse a single monkey block into a map."
  [block]
  (let [lines (str/split-lines block)
        items (vec (parse-numbers (nth lines 1)))
        op-match (re-find #"new = old ([+*]) (\w+)" (nth lines 2))
        operator (second op-match)
        operand (nth op-match 2)
        divisor (first (parse-numbers (nth lines 3)))
        if-true (first (parse-numbers (nth lines 4)))
        if-false (first (parse-numbers (nth lines 5)))]
    {:items items
     :operator operator
     :operand operand
     :divisor divisor
     :if-true if-true
     :if-false if-false
     :inspections 0}))

(defn parse-monkeys
  "Parse all monkey definitions from input text."
  [text]
  (vec (map parse-monkey (str/split text #"\n\n"))))

(defn apply-operation
  "Apply the monkey's operation to a worry level."
  [old operator operand]
  (let [val (if (= operand "old") old (Long/parseLong operand))]
    (if (= operator "+")
      (+ old val)
      (* old val))))

(defn process-item
  "Process a single item for a monkey, returning [target-monkey new-worry-level]."
  [item monkey relief-divisor mod-value]
  (let [{:keys [operator operand divisor if-true if-false]} monkey
        new-val (apply-operation item operator operand)
        new-val (if (> relief-divisor 1)
                  (quot new-val relief-divisor)
                  new-val)
        new-val (if mod-value
                  (mod new-val mod-value)
                  new-val)
        target (if (zero? (mod new-val divisor)) if-true if-false)]
    [target new-val]))

(defn process-monkey
  "Process all items for a single monkey, returning updated monkeys vector."
  [monkeys idx relief-divisor mod-value]
  (let [monkey (nth monkeys idx)
        items (:items monkey)]
    (if (empty? items)
      monkeys
      (let [item (first items)
            [target new-val] (process-item item monkey relief-divisor mod-value)
            monkeys (update-in monkeys [idx :items] #(vec (rest %)))
            monkeys (update-in monkeys [idx :inspections] inc)
            monkeys (update-in monkeys [target :items] conj new-val)]
        (recur monkeys idx relief-divisor mod-value)))))

(defn run-round
  "Run a single round of monkey business."
  [monkeys relief-divisor mod-value]
  (reduce (fn [m idx] (process-monkey m idx relief-divisor mod-value))
          monkeys
          (range (count monkeys))))

(defn simulate
  "Simulate monkey business for given number of rounds."
  [monkeys rounds relief-divisor use-modulo]
  (let [mod-value (when use-modulo
                    (reduce * (map :divisor monkeys)))]
    (reduce (fn [m _] (run-round m relief-divisor mod-value))
            monkeys
            (range rounds))))

(defn monkey-business
  "Calculate monkey business: product of top 2 inspection counts."
  [monkeys]
  (let [inspections (sort > (map :inspections monkeys))]
    (* (first inspections) (second inspections))))

(defn part1
  "Run 20 rounds with relief (divide by 3)."
  [text]
  (let [monkeys (parse-monkeys text)
        result (simulate monkeys 20 3 false)]
    (monkey-business result)))

(defn part2
  "Run 10000 rounds without relief."
  [text]
  (let [monkeys (parse-monkeys text)
        result (simulate monkeys 10000 1 true)]
    (monkey-business result)))

(defn -main []
  (let [input-file (or (first *command-line-args*) "../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
