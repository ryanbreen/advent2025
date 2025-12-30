#!/usr/bin/env -S clojure -M

(ns solution
  (:require [clojure.string :as str]))

(defn hash-algorithm
  "Run the HASH algorithm on a string."
  [s]
  (reduce (fn [current c]
            (-> current
                (+ (int c))
                (* 17)
                (mod 256)))
          0
          s))

(defn part1
  "Sum of HASH values for all steps."
  [steps]
  (reduce + (map hash-algorithm steps)))

(defn parse-step
  "Parse a step into [label operation focal-length]."
  [step]
  (if (str/includes? step "=")
    (let [[label focal] (str/split step #"=")]
      [label := (parse-long focal)])
    [(subs step 0 (dec (count step))) :- nil]))

(defn process-step
  "Process a single step, updating boxes."
  [boxes step]
  (let [[label op focal] (parse-step step)
        box-num (hash-algorithm label)]
    (if (= op :=)
      ;; Add or replace lens
      (let [box (get boxes box-num [])
            idx (first (keep-indexed (fn [i [l _]] (when (= l label) i)) box))]
        (if idx
          ;; Replace existing lens
          (assoc boxes box-num (assoc box idx [label focal]))
          ;; Add new lens
          (assoc boxes box-num (conj box [label focal]))))
      ;; Remove lens
      (let [box (get boxes box-num [])]
        (assoc boxes box-num (vec (remove #(= (first %) label) box)))))))

(defn focusing-power
  "Calculate the focusing power of all lenses."
  [boxes]
  (reduce-kv
   (fn [total box-num box]
     (+ total
        (reduce-kv
         (fn [box-total slot [_ focal]]
           (+ box-total (* (inc box-num) (inc slot) focal)))
         0
         (vec box))))
   0
   boxes))

(defn part2
  "Run HASHMAP procedure and calculate focusing power."
  [steps]
  (let [final-boxes (reduce process-step {} steps)]
    (focusing-power final-boxes)))

(defn -main []
  (let [input-file (-> *file*
                       (java.io.File.)
                       (.getParent)
                       (str "/../input.txt"))
        text (str/trim (slurp input-file))
        steps (str/split text #",")]
    (println "Part 1:" (part1 steps))
    (println "Part 2:" (part2 steps))))

(-main)
