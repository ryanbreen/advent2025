#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(def input-file (str (-> *file* java.io.File. .getParent) "/../input.txt"))

(defn parse-sensors [text]
  "Parse sensor and beacon positions from input text."
  (let [pattern #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"]
    (->> (str/split-lines text)
         (filter #(not (str/blank? %)))
         (map (fn [line]
                (let [[_ sx sy bx by] (re-matches pattern line)
                      sx (parse-long sx)
                      sy (parse-long sy)
                      bx (parse-long bx)
                      by (parse-long by)
                      dist (+ (abs (- sx bx)) (abs (- sy by)))]
                  {:sx sx :sy sy :bx bx :by by :dist dist}))))))

(defn merge-ranges [ranges]
  "Merge overlapping ranges."
  (if (empty? ranges)
    []
    (let [sorted (sort-by first ranges)]
      (reduce (fn [merged [start end]]
                (let [[m-start m-end] (last merged)]
                  (if (<= start (inc m-end))
                    (conj (vec (butlast merged)) [m-start (max m-end end)])
                    (conj merged [start end]))))
              [(first sorted)]
              (rest sorted)))))

(defn get-coverage-at-row [sensors row]
  "Get merged ranges covered by sensors at a specific row."
  (let [ranges (for [{:keys [sx sy dist]} sensors
                     :let [row-dist (abs (- sy row))]
                     :when (<= row-dist dist)
                     :let [x-spread (- dist row-dist)]]
                 [(- sx x-spread) (+ sx x-spread)])]
    (merge-ranges ranges)))

(defn part1 [sensors]
  "Count positions that cannot contain a beacon at row y=2000000."
  (let [target-row 2000000
        ranges (get-coverage-at-row sensors target-row)
        total (reduce + (map (fn [[start end]] (inc (- end start))) ranges))
        beacons-on-row (->> sensors
                            (filter #(= (:by %) target-row))
                            (map :bx)
                            set
                            count)]
    (- total beacons-on-row)))

(defn part2 [sensors]
  "Find the distress beacon's tuning frequency."
  (let [max-coord 4000000]
    (loop [row 0]
      (if (> row max-coord)
        nil
        (let [ranges (get-coverage-at-row sensors row)
              clipped (->> ranges
                           (filter (fn [[start end]]
                                     (and (>= end 0) (<= start max-coord))))
                           (map (fn [[start end]]
                                  [(max 0 start) (min max-coord end)]))
                           merge-ranges)]
          (if (and (= 1 (count clipped))
                   (= 0 (first (first clipped)))
                   (= max-coord (second (first clipped))))
            (recur (inc row))
            ;; Found a gap
            (let [x (if (> (count clipped) 1)
                      (inc (second (first clipped)))
                      (if (> (first (first clipped)) 0)
                        0
                        (inc (second (first clipped)))))]
              (+ (* x 4000000) row))))))))

(defn -main []
  (let [text (slurp input-file)
        sensors (parse-sensors text)]
    (println "Part 1:" (part1 sensors))
    (println "Part 2:" (part2 sensors))))

(-main)
