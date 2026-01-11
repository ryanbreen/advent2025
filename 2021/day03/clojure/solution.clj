(ns solution
  (:require [clojure.string :as str]))

(defn parse-input []
  (let [input-path (str (System/getProperty "user.dir") "/../input.txt")]
    (->> (slurp input-path)
         str/split-lines
         (filter (complement str/blank?)))))

(defn count-ones-at-pos [numbers pos]
  (count (filter #(= \1 (nth % pos)) numbers)))

(defn part1 [numbers]
  (let [num-bits (count (first numbers))
        n (count numbers)
        gamma (reduce (fn [acc pos]
                        (let [ones (count-ones-at-pos numbers pos)
                              zeros (- n ones)]
                          (if (>= ones zeros)
                            (bit-or acc (bit-shift-left 1 (- num-bits 1 pos)))
                            acc)))
                      0
                      (range num-bits))
        mask (dec (bit-shift-left 1 num-bits))
        epsilon (bit-xor gamma mask)]
    (* gamma epsilon)))

(defn find-rating [numbers use-most-common]
  (let [num-bits (count (first numbers))]
    (loop [candidates numbers
           pos 0]
      (if (or (= 1 (count candidates)) (>= pos num-bits))
        (Integer/parseInt (first candidates) 2)
        (let [ones (count-ones-at-pos candidates pos)
              zeros (- (count candidates) ones)
              target (if use-most-common
                       (if (>= ones zeros) \1 \0)
                       (if (<= zeros ones) \0 \1))]
          (recur (filter #(= target (nth % pos)) candidates)
                 (inc pos)))))))

(defn part2 [numbers]
  (let [oxygen (find-rating numbers true)
        co2 (find-rating numbers false)]
    (* oxygen co2)))

(defn -main []
  (let [numbers (parse-input)]
    (println (str "Part 1: " (part1 numbers)))
    (println (str "Part 2: " (part2 numbers)))))

(-main)
