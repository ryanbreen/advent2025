(require '[clojure.set :as set]
         '[clojure.string :as str])

(defn parse-input [filename]
  (->> (slurp filename)
       str/split-lines
       (filter (complement str/blank?))))

(defn priority [c]
  (let [code (int c)]
    (if (Character/isLowerCase c)
      (+ 1 (- code (int \a)))
      (+ 27 (- code (int \A))))))

(defn part1 [rucksacks]
  (->> rucksacks
       (map (fn [rucksack]
              (let [mid (/ (count rucksack) 2)
                    first-half (set (take mid rucksack))
                    second-half (set (drop mid rucksack))
                    common (first (set/intersection first-half second-half))]
                (priority common))))
       (reduce +)))

(defn part2 [rucksacks]
  (->> rucksacks
       (partition 3)
       (map (fn [[a b c]]
              (let [common (first (set/intersection (set a) (set b) (set c)))]
                (priority common))))
       (reduce +)))

(defn -main []
  (let [input-file "../input.txt"
        rucksacks (parse-input input-file)]
    (println "Part 1:" (part1 rucksacks))
    (println "Part 2:" (part2 rucksacks))))

(-main)
