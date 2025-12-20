(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [input-text]
  (let [parts (str/split (str/trim input-text) #"\n\n")
        patterns (->> (str/split (first parts) #",")
                      (map str/trim)
                      vec)
        designs (str/split-lines (second parts))]
    {:patterns patterns
     :designs designs}))

(defn count-ways
  "Count ways to form design from patterns using bottom-up dynamic programming.
   dp[i] = number of ways to form design[i:] from patterns."
  [design patterns]
  (let [n (count design)]
    (-> (reduce
          (fn [dp pos]
            (let [ways (reduce
                         (fn [total pattern]
                           (let [end (+ pos (count pattern))]
                             (if (and (<= end n)
                                      (= pattern (subs design pos end)))
                               (+ total (dp end))
                               total)))
                         0
                         patterns)]
              (assoc dp pos ways)))
          (assoc (vec (repeat (inc n) 0)) n 1)
          (range (dec n) -1 -1))
        (get 0))))

(defn part1 [designs patterns]
  (->> designs
       (filter #(pos? (count-ways % patterns)))
       count))

(defn part2 [designs patterns]
  (transduce (map #(count-ways % patterns)) + designs))

(defn -main [& _args]
  (let [{:keys [patterns designs]} (parse-input (slurp "../input.txt"))]
    (println "Part 1:" (part1 designs patterns))
    (println "Part 2:" (part2 designs patterns))))

(-main)
