(ns solution
  (:require [clojure.string :as str]))

;; Card strength order (higher index = stronger)
(def card-strength "23456789TJQKA")
(def card-strength-joker "J23456789TQKA")  ; J is weakest in Part 2

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #"\s+"))
       (map (fn [[hand bid]] [hand (parse-long bid)]))))

(defn get-hand-type
  "Return hand type as integer (higher = stronger).
   6=five-of-a-kind, 5=four-of-a-kind, 4=full-house,
   3=three-of-a-kind, 2=two-pair, 1=one-pair, 0=high-card"
  [hand]
  (let [counts (->> hand
                    frequencies
                    vals
                    (sort >)
                    vec)]
    (condp = counts
      [5]          6  ; Five of a kind
      [4 1]        5  ; Four of a kind
      [3 2]        4  ; Full house
      [3 1 1]      3  ; Three of a kind
      [2 2 1]      2  ; Two pair
      [2 1 1 1]    1  ; One pair
      [1 1 1 1 1]  0  ; High card
      0)))            ; Default fallback

(defn hand-key
  "Return sort key for a hand (type, then card strengths)."
  [hand]
  (let [hand-type (get-hand-type hand)
        card-values (mapv #(str/index-of card-strength (str %)) hand)]
    [hand-type card-values]))

(defn get-hand-type-with-jokers
  "Return hand type with J as wildcards (higher = stronger)."
  [hand]
  (let [joker-count (count (filter #(= % \J) hand))]
    (cond
      (= joker-count 0) (get-hand-type hand)
      (= joker-count 5) 6  ; Five of a kind
      :else
      (let [non-jokers (filter #(not= % \J) hand)
            counts (->> non-jokers
                        frequencies
                        vals
                        (sort >)
                        vec)
            ;; Add jokers to the highest count
            adjusted-counts (update counts 0 + joker-count)]
        (condp = adjusted-counts
          [5]          6  ; Five of a kind
          [4 1]        5  ; Four of a kind
          [3 2]        4  ; Full house
          [3 1 1]      3  ; Three of a kind
          [2 2 1]      2  ; Two pair
          [2 1 1 1]    1  ; One pair
          [1 1 1 1 1]  0  ; High card
          0)))))

(defn hand-key-with-jokers
  "Return sort key for a hand with joker rules."
  [hand]
  (let [hand-type (get-hand-type-with-jokers hand)
        card-values (mapv #(str/index-of card-strength-joker (str %)) hand)]
    [hand-type card-values]))

(defn calculate-winnings
  "Calculate total winnings by sorting hands and multiplying by rank."
  [hands key-fn]
  (->> hands
       (sort-by (fn [[hand _]] (key-fn hand)))
       (map-indexed (fn [idx [_ bid]] (* (inc idx) bid)))
       (reduce +)))

(defn part1 [hands]
  (calculate-winnings hands hand-key))

(defn part2 [hands]
  (calculate-winnings hands hand-key-with-jokers))

(defn -main []
  (let [input (slurp "../input.txt")
        hands (parse-input input)]
    (println "Part 1:" (part1 hands))
    (println "Part 2:" (part2 hands))))

(-main)
