#!/usr/bin/env clojure

(require '[clojure.string :as str])

(def words
  {"one" "1", "two" "2", "three" "3", "four" "4", "five" "5"
   "six" "6", "seven" "7", "eight" "8", "nine" "9"})

(defn read-lines []
  (-> "../input.txt"
      slurp
      str/trim
      str/split-lines))

(defn extract-digits [line digit-fn]
  (->> (range (count line))
       (keep #(digit-fn line %))
       ((juxt first last))))

(defn calibration-value [[first-digit last-digit]]
  (parse-long (str first-digit last-digit)))

(defn numeric-digit-at [line pos]
  (let [ch (nth line pos)]
    (when (Character/isDigit ch)
      (str ch))))

(defn digit-or-word-at [line pos]
  (let [ch (nth line pos)]
    (cond
      (Character/isDigit ch)
      (str ch)

      :else
      (some (fn [[word digit]]
              (when (str/starts-with? (subs line pos) word)
                digit))
            words))))

(defn solve [lines digit-fn]
  (->> lines
       (map #(extract-digits % digit-fn))
       (map calibration-value)
       (reduce +)))

(defn part1 []
  (solve (read-lines) numeric-digit-at))

(defn part2 []
  (solve (read-lines) digit-or-word-at))

(defn -main []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))

(-main)
