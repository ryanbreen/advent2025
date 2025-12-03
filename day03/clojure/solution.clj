#!/usr/bin/env clojure

(require '[clojure.string :as str])

;; Read and parse input
(def input-text (slurp "../input.txt"))
(def lines (str/split-lines (str/trim input-text)))

;; Part 1: Select exactly 2 batteries to maximize 2-digit number
(defn part1 []
  (let [total (atom 0)]
    (doseq [line lines]
      (let [n (count line)
            ;; Build max-suffix array: max digit from position i to end
            max-suffix (vec (reverse
                              (reductions max
                                          (reverse (map #(Character/digit % 10) line)))))
            max-joltage (atom 0)]
        ;; Try each position for first battery
        (doseq [i (range (dec n))]
          (let [first-digit (Character/digit (nth line i) 10)
                ;; Maximum second digit from remaining positions
                max-second (nth max-suffix (inc i))
                joltage (+ (* first-digit 10) max-second)]
            (swap! max-joltage max joltage)))
        (swap! total + @max-joltage)))
    @total))

;; Part 2: Select exactly 12 batteries to maximize 12-digit number
(defn part2 []
  (let [total (atom 0)
        k 12] ;; Select exactly 12 batteries
    (doseq [line lines]
      (let [n (count line)
            result (atom [])
            current-pos (atom 0)]
        ;; Greedy algorithm: select k digits that form maximum number
        (dotimes [i k]
          (let [remaining-needed (- k i 1)
                search-end (- n remaining-needed)
                ;; Find maximum digit in valid range
                max-info (reduce
                           (fn [acc j]
                             (let [digit (Character/digit (nth line j) 10)]
                               (if (> digit (:digit acc))
                                 {:digit digit :pos j}
                                 acc)))
                           {:digit -1 :pos @current-pos}
                           (range @current-pos search-end))]
            (swap! result conj (:digit max-info))
            (reset! current-pos (inc (:pos max-info)))))
        ;; Convert result to long
        (let [joltage (Long/parseLong (str/join "" @result))]
          (swap! total + joltage))))
    @total))

;; Main execution
(defn -main []
  (println (str "Part 1: " (part1)))
  (println (str "Part 2: " (part2))))

(-main)
