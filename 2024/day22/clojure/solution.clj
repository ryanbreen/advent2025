#!/usr/bin/env clojure
;;; Day 22: Monkey Market - Pseudorandom number generation for market prices

(require '[clojure.string :as str])

(defn next-secret
  "Generate the next secret number using mix and prune operations."
  [secret]
  (let [;; Step 1: multiply by 64 (* 64 = << 6), mix (XOR), prune (& 0xFFFFFF)
        s1 (bit-and (bit-xor secret (bit-shift-left secret 6)) 0xFFFFFF)
        ;; Step 2: divide by 32 (/ 32 = >> 5), mix, prune
        s2 (bit-and (bit-xor s1 (bit-shift-right s1 5)) 0xFFFFFF)
        ;; Step 3: multiply by 2048 (* 2048 = << 11), mix, prune
        s3 (bit-and (bit-xor s2 (bit-shift-left s2 11)) 0xFFFFFF)]
    s3))

(defn generate-secrets
  "Generate a sequence of secret numbers."
  [initial count]
  (take (inc count)
        (iterate next-secret initial)))

(defn part1
  "Sum of the 2000th secret number for each buyer."
  [initial-secrets]
  (->> initial-secrets
       (map #(nth (iterate next-secret %) 2000))
       (reduce +)))

(defn part2
  "Find the best sequence of 4 price changes to maximize bananas."
  [initial-secrets]
  (let [;; For each buyer, generate secrets and track sequences
        sequence-totals
        (reduce
          (fn [totals initial]
            (let [;; Generate 2001 secrets (initial + 2000 new)
                  secrets (vec (generate-secrets initial 2000))
                  ;; Get prices (last digit of each secret)
                  prices (mapv #(mod % 10) secrets)
                  ;; Calculate changes between consecutive prices
                  changes (mapv - (rest prices) prices)
                  ;; Find all 4-change sequences and track first occurrence
                  sequences (for [i (range (- (count changes) 3))]
                              [(vec (subvec changes i (+ i 4))) (nth prices (+ i 4))])]
              ;; For this buyer, track only first occurrence of each sequence
              (:totals
                (reduce
                  (fn [acc [seq price]]
                    (if (contains? (:seen acc) seq)
                      acc
                      (-> acc
                          (update :seen conj seq)
                          (update :totals (fn [t] (update t seq (fnil + 0) price))))))
                  {:seen #{} :totals totals}
                  sequences))))
          {}
          initial-secrets)]
    ;; Return maximum value across all sequences
    (->> sequence-totals
         vals
         (apply max))))

(defn -main []
  (let [input (slurp "../input.txt")
        initial-secrets (->> (str/split-lines input)
                            (map str/trim)
                            (filter seq)
                            (map parse-long))]
    (println (str "Part 1: " (part1 initial-secrets)))
    (println (str "Part 2: " (part2 initial-secrets)))))

(-main)
