#!/usr/bin/env clojure

(ns day10.solution
  (:require [clojure.string :as str]))

;; ===== PARSING =====

(defn parse-line-part1 [line]
  "Parse a machine line for Part 1 - extract indicator pattern and buttons."
  (let [indicator-match (re-find #"\[([.#]+)\]" line)
        indicator (second indicator-match)
        n-lights (count indicator)

        ;; Target state: 1 where # appears
        target (reduce (fn [acc [i c]]
                         (if (= c \#)
                           (bit-or acc (bit-shift-left 1 i))
                           acc))
                       0
                       (map-indexed vector indicator))

        ;; Extract button schematics
        buttons (mapv (fn [match]
                        (let [indices (mapv #(Integer/parseInt %)
                                          (str/split (second match) #","))]
                          (reduce (fn [mask idx]
                                    (bit-or mask (bit-shift-left 1 idx)))
                                  0
                                  indices)))
                      (re-seq #"\(([0-9,]+)\)" line))]
    [n-lights target buttons]))

(defn parse-line-part2 [line]
  "Parse a machine line for Part 2 - extract joltage requirements and buttons."
  (let [joltage-match (re-find #"\{([0-9,]+)\}" line)
        joltage (mapv #(Integer/parseInt %)
                     (str/split (second joltage-match) #","))
        n-counters (count joltage)

        ;; Extract button schematics as lists of indices
        buttons (mapv (fn [match]
                        (mapv #(Integer/parseInt %)
                             (str/split (second match) #",")))
                      (re-seq #"\(([0-9,]+)\)" line))]
    [n-counters joltage buttons]))

;; ===== PART 1: BRUTE FORCE XOR =====

(defn count-bits [n]
  "Count number of 1 bits in n."
  (loop [x n cnt 0]
    (if (zero? x)
      cnt
      (recur (bit-shift-right x 1)
             (+ cnt (bit-and x 1))))))

(defn solve-machine-part1 [n-lights target buttons]
  "Brute force: try all 2^n button combinations."
  (let [n-buttons (count buttons)]
    (loop [mask 0 min-presses Long/MAX_VALUE]
      (if (>= mask (bit-shift-left 1 n-buttons))
        (if (= min-presses Long/MAX_VALUE) 0 min-presses)
        (let [state (reduce (fn [s i]
                              (if (bit-test mask i)
                                (bit-xor s (buttons i))
                                s))
                            0
                            (range n-buttons))
              presses (count-bits mask)]
          (recur (inc mask)
                 (if (= state target)
                   (min min-presses presses)
                   min-presses)))))))

;; ===== PART 2: GAUSSIAN ELIMINATION WITH RATIOS =====

(defn mapv-indexed [f coll]
  "Like mapv but with index."
  (vec (map-indexed f coll)))

(defn is-nonneg-int? [x]
  "Check if x is a non-negative integer (works with Ratio and BigInt)."
  (and (>= x 0)
       (or (integer? x)
           (and (ratio? x) (= 1 (denominator x))))))

(defn to-int [x]
  "Convert rational to integer (works with Ratio and BigInt)."
  (if (ratio? x)
    (numerator x)
    x))

(defn gaussian-elimination-rref [A b]
  "Gaussian elimination to RREF. Returns augmented matrix in RREF.
   A is n-rows x n-cols matrix, b is n-rows vector.
   Uses Clojure's Ratio type for exact rational arithmetic."
  (let [n-rows (count A)
        n-cols (count (first A))
        ;; Create augmented matrix [A | b] with ratios
        aug (vec (map-indexed (fn [i row]
                                (vec (concat (map #(rationalize %) row)
                                           [(rationalize (b i))])))
                              A))]
    (loop [pivot-row 0 col 0 aug aug pivot-cols []]
      (if (or (>= pivot-row n-rows) (>= col n-cols))
        {:aug aug :pivot-cols pivot-cols}

        ;; Find non-zero entry in this column
        (let [found (first (filter #(not= 0 (get-in aug [% col]))
                                  (range pivot-row n-rows)))]
          (if (nil? found)
            ;; No pivot, move to next column
            (recur pivot-row (inc col) aug pivot-cols)

            ;; Swap rows
            (let [aug (if (= found pivot-row)
                       aug
                       (assoc aug pivot-row (aug found)
                                  found (aug pivot-row)))

                  ;; Scale pivot row
                  scale (get-in aug [pivot-row col])
                  aug (update aug pivot-row
                             (fn [row]
                               (mapv #(/ % scale) row)))

                  ;; Eliminate column in all other rows
                  aug (reduce (fn [aug row]
                               (if (or (= row pivot-row)
                                      (= 0 (get-in aug [row col])))
                                 aug
                                 (let [factor (get-in aug [row col])]
                                   (update aug row
                                          (fn [r]
                                            (mapv-indexed
                                             (fn [c v]
                                               (- v (* factor (get-in aug [pivot-row c]))))
                                             r))))))
                             aug
                             (range n-rows))]
              (recur (inc pivot-row) (inc col) aug
                     (conj pivot-cols [col pivot-row])))))))))

(defn solve-machine-part2 [n-counters joltage buttons]
  "Solve Part 2 using Gaussian elimination and null space search."
  (let [n-buttons (count buttons)]
    (if (zero? n-buttons)
      (if (every? zero? joltage) 0 Long/MAX_VALUE)

      ;; Build matrix A (n_counters x n_buttons)
      (let [A (vec (for [i (range n-counters)]
                     (vec (for [j (range n-buttons)]
                            (if (some #(= % i) (buttons j)) 1 0)))))
            b (vec joltage)

            ;; Gaussian elimination
            {:keys [aug pivot-cols]} (gaussian-elimination-rref A b)

            ;; Check for inconsistency
            inconsistent? (some (fn [row-idx]
                                 (let [row (aug row-idx)]
                                   (and (every? zero? (take n-buttons row))
                                        (not= 0 (last row)))))
                               (range (count pivot-cols) n-counters))]

        (if inconsistent?
          Long/MAX_VALUE

          ;; Identify free variables
          (let [pivot-col-set (set (map first pivot-cols))
                free-vars (vec (filter #(not (pivot-col-set %))
                                      (range n-buttons)))]

            (if (empty? free-vars)
              ;; Unique solution
              (let [solution (vec (repeat n-buttons 0N))
                    solution (reduce (fn [sol [col row]]
                                      (assoc sol col (last (aug row))))
                                    solution
                                    pivot-cols)]
                (if (every? is-nonneg-int? solution)
                  (reduce + (map to-int solution))
                  Long/MAX_VALUE))

              ;; With free variables, search for optimal solution
              (let [;; Extract null space vectors
                    null-vectors (mapv (fn [fv]
                                        (let [vec (vec (repeat n-buttons 0N))
                                              vec (assoc vec fv 1N)]
                                          (reduce (fn [v [col row]]
                                                   (assoc v col (- (get-in aug [row fv]))))
                                                 vec
                                                 pivot-cols)))
                                      free-vars)

                    ;; Extract particular solution
                    particular (reduce (fn [sol [col row]]
                                        (assoc sol col (last (aug row))))
                                      (vec (repeat n-buttons 0N))
                                      pivot-cols)

                    ;; Search for optimal solution
                    max-j (apply max joltage)
                    n-free (count free-vars)]

                (cond
                  ;; 1D search
                  (= n-free 1)
                  (let [nv (first null-vectors)
                        ;; Compute bounds
                        [t-low t-high]
                        (reduce (fn [[low high] j]
                                 (let [p (particular j)
                                       nv-j (nv j)]
                                   (cond
                                     (zero? nv-j) (if (< p 0)
                                                   [Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY]
                                                   [low high])
                                     (pos? nv-j) [(max low (/ (- p) nv-j)) high]
                                     :else [low (min high (/ (- p) nv-j))])))
                               [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
                               (range n-buttons))

                        t-low-int (long (Math/ceil (double t-low)))
                        t-high-int (long (Math/floor (double t-high)))]

                    (loop [t t-low-int min-total Long/MAX_VALUE]
                      (if (> t t-high-int)
                        (if (= min-total Long/MAX_VALUE) 0 min-total)
                        (let [solution (mapv (fn [j]
                                              (+ (particular j) (* t (get nv j))))
                                            (range n-buttons))
                              valid? (every? is-nonneg-int? solution)]
                          (recur (inc t)
                                 (if valid?
                                   (min min-total (reduce + (map to-int solution)))
                                   min-total))))))

                  ;; 2D search
                  (= n-free 2)
                  (let [nv0 (nth null-vectors 0)
                        nv1 (nth null-vectors 1)
                        bound max-j]
                    (loop [t0 (- bound) min-total Long/MAX_VALUE]
                      (if (> t0 bound)
                        (if (= min-total Long/MAX_VALUE) 0 min-total)
                        (let [inter (mapv (fn [j]
                                           (+ (particular j) (* t0 (nv0 j))))
                                         (range n-buttons))

                              ;; Compute bounds for t1
                              [t1-low t1-high]
                              (reduce (fn [[low high] j]
                                       (let [p (inter j)
                                             nv-j (nv1 j)]
                                         (cond
                                           (zero? nv-j) [low high]
                                           (pos? nv-j) [(max low (/ (- p) nv-j)) high]
                                           :else [low (min high (/ (- p) nv-j))])))
                                     [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
                                     (range n-buttons))

                              t1-low-int (long (Math/ceil (double t1-low)))
                              t1-high-int (long (Math/floor (double t1-high)))]

                          (recur (inc t0)
                                 (loop [t1 t1-low-int min-t min-total]
                                   (if (> t1 t1-high-int)
                                     min-t
                                     (let [solution (mapv (fn [j]
                                                           (+ (inter j) (* t1 (nv1 j))))
                                                         (range n-buttons))
                                           valid? (every? is-nonneg-int? solution)]
                                       (recur (inc t1)
                                              (if valid?
                                                (min min-t (reduce + (map to-int solution)))
                                                min-t))))))))))

                  ;; 3D search with widened t1 bounds
                  (= n-free 3)
                  (let [bound max-j
                        nv0 (nth null-vectors 0)
                        nv1 (nth null-vectors 1)
                        nv2 (nth null-vectors 2)]
                    (loop [t0 (- bound) min-total Long/MAX_VALUE]
                      (if (> t0 bound)
                        (if (= min-total Long/MAX_VALUE) 0 min-total)
                        (let [;; Compute intermediate after t0
                              inter0 (mapv (fn [j]
                                            (+ (particular j) (* t0 (nv0 j))))
                                          (range n-buttons))

                              ;; Compute bounds for t1 with padding
                              [t1-low t1-high]
                              (reduce (fn [[low high] j]
                                       (let [p (inter0 j)
                                             nv-j (nv1 j)]
                                         (cond
                                           (zero? nv-j) [low high]
                                           ;; Add padding for fractional coefficients
                                           (pos? nv-j) [(max low (- (/ (- p) nv-j) bound)) high]
                                           :else [low (min high (+ (/ (- p) nv-j) bound))])))
                                     [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
                                     (range n-buttons))

                              t1-low-int (max (long (Math/ceil (double t1-low)))
                                            (- bound))
                              t1-high-int (min (long (Math/floor (double t1-high)))
                                             bound)]

                          (recur (inc t0)
                                 (loop [t1 t1-low-int min-t min-total]
                                   (if (> t1 t1-high-int)
                                     min-t
                                     (let [inter1 (mapv (fn [j]
                                                         (+ (inter0 j) (* t1 (nv1 j))))
                                                       (range n-buttons))

                                           ;; Compute tight bounds for t2
                                           [t2-low t2-high]
                                           (reduce (fn [[low high] j]
                                                    (let [p (inter1 j)
                                                          nv-j (nv2 j)]
                                                      (cond
                                                        (zero? nv-j) [low high]
                                                        (pos? nv-j) [(max low (/ (- p) nv-j)) high]
                                                        :else [low (min high (/ (- p) nv-j))])))
                                                  [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
                                                  (range n-buttons))

                                           t2-low-int (long (Math/ceil (double t2-low)))
                                           t2-high-int (long (Math/floor (double t2-high)))]

                                       (recur (inc t1)
                                              (loop [t2 t2-low-int min-t2 min-t]
                                                (if (> t2 t2-high-int)
                                                  min-t2
                                                  (let [solution (mapv (fn [j]
                                                                        (+ (inter1 j) (* t2 (nv2 j))))
                                                                      (range n-buttons))
                                                        valid? (every? is-nonneg-int? solution)]
                                                    (recur (inc t2)
                                                           (if valid?
                                                             (min min-t2 (reduce + (map to-int solution)))
                                                             min-t2))))))))))))))

                  ;; 4+D: use recursive search with widened bounds
                  (<= n-free 6)
                  (let [bound (* 2 max-j)]
                    (letfn [(search [idx partial min-total]
                              (if (= idx n-free)
                                ;; Evaluate solution
                                (if (every? is-nonneg-int? partial)
                                  (min min-total (reduce + (map to-int partial)))
                                  min-total)

                                ;; Compute bounds for current free var with padding
                                (let [nv (nth null-vectors idx)
                                      [t-low t-high]
                                      (reduce (fn [[low high] j]
                                               (let [p (partial j)
                                                     nv-j (nv j)]
                                                 (cond
                                                   (zero? nv-j) [low high]
                                                   (pos? nv-j) [(max low (/ (- p) nv-j)) high]
                                                   :else [low (min high (/ (- p) nv-j))])))
                                             [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
                                             (range n-buttons))

                                      ;; Widen bounds to account for integrality
                                      t-low-int (max (long (Math/ceil (- (double t-low) max-j)))
                                                    (- (* 2 max-j)))
                                      t-high-int (min (long (Math/floor (+ (double t-high) max-j)))
                                                     (* 2 max-j))]

                                  (loop [t t-low-int min-t min-total]
                                    (if (> t t-high-int)
                                      min-t
                                      (let [new-partial (mapv (fn [j]
                                                               (+ (partial j) (* t (nv j))))
                                                             (range n-buttons))]
                                        (recur (inc t)
                                               (search (inc idx) new-partial min-t))))))))]
                      (let [result (search 0 particular Long/MAX_VALUE)]
                        (if (= result Long/MAX_VALUE) 0 result))))

                  :else 0)))))))))

;; ===== MAIN =====

(defn part1 [lines]
  "Find minimum total button presses for all machines."
  (reduce (fn [total line]
            (if (str/blank? line)
              total
              (let [[n-lights target buttons] (parse-line-part1 line)]
                (+ total (solve-machine-part1 n-lights target buttons)))))
          0
          lines))

(defn part2 [lines]
  "Find minimum total button presses for joltage configuration."
  (reduce (fn [total line]
            (if (str/blank? line)
              total
              (let [[n-counters joltage buttons] (parse-line-part2 line)]
                (+ total (solve-machine-part2 n-counters joltage buttons)))))
          0
          lines))

(defn -main []
  (let [input (slurp "../input.txt")
        lines (str/split-lines input)]
    (println (str "Part 1: " (part1 lines)))
    (println (str "Part 2: " (part2 lines)))))

(-main)
