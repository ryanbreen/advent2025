#!/usr/bin/env clojure

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-input [filename]
  "Parse initial wire values and gates from input file."
  (let [content (slurp filename)
        [wires-section gates-section] (str/split content #"\n\n")

        ;; Parse initial wire values
        wires (into {}
                (for [line (str/split-lines wires-section)
                      :let [[name val] (str/split line #": ")]]
                  [name (parse-long val)]))

        ;; Parse gates: "x00 AND y00 -> z00"
        gates (for [line (str/split-lines gates-section)
                    :let [parts (str/split line #" ")
                          in1 (nth parts 0)
                          op (nth parts 1)
                          in2 (nth parts 2)
                          out (nth parts 4)]]
                [in1 op in2 out])]
    [wires gates]))

(defn apply-gate [op v1 v2]
  "Apply a gate operation to two values."
  (case op
    "AND" (bit-and v1 v2)
    "OR" (bit-or v1 v2)
    "XOR" (bit-xor v1 v2)))

(defn simulate [initial-wires gates]
  "Simulate the circuit until all outputs are computed."
  (loop [wires initial-wires
         remaining gates]
    (if (empty? remaining)
      wires
      (let [;; Try to process gates
            [processed unprocessed new-wires]
            (reduce (fn [[proc unproc wires] [in1 op in2 out]]
                      (if (and (contains? wires in1) (contains? wires in2))
                        [(conj proc [in1 op in2 out])
                         unproc
                         (assoc wires out (apply-gate op (wires in1) (wires in2)))]
                        [proc (conj unproc [in1 op in2 out]) wires]))
                    [[] [] wires]
                    remaining)]
        (if (and (empty? processed) (not (empty? unprocessed)))
          (throw (ex-info "Circuit stuck - missing inputs" {}))
          (recur new-wires unprocessed))))))

(defn get-z-value [wires]
  "Extract the decimal number from z wires."
  (let [z-wires (sort #(compare %2 %1) (filter #(str/starts-with? % "z") (keys wires)))]
    (reduce (fn [result z]
              (bit-or (bit-shift-left result 1) (wires z)))
            0
            z-wires)))

(defn part1 [wires gates]
  "Simulate and get the z output."
  (let [final-wires (simulate wires gates)]
    (get-z-value final-wires)))

(defn part2 [gates]
  "Find the 8 swapped wires in the adder circuit.

  A correct ripple-carry adder has this structure:
  - Bit 0: z00 = x00 XOR y00, carry0 = x00 AND y00
  - Bit i: z[i] = (x[i] XOR y[i]) XOR carry[i-1]
           carry[i] = (x[i] AND y[i]) OR ((x[i] XOR y[i]) AND carry[i-1])

  We check structural rules to find violations."
  (let [;; Build lookup maps
        gate-by-output (into {} (for [[in1 op in2 out] gates]
                                  [out [in1 op in2]]))

        gate-by-inputs-op (into {} (for [[in1 op in2 out] gates]
                                     [[(set [in1 in2]) op] out]))

        ;; Find max bit
        max-bit (apply max (for [[_ _ _ out] gates
                                 :when (str/starts-with? out "z")]
                             (parse-long (subs out 1))))

        max-z (format "z%02d" max-bit)

        swapped (atom #{})]

    ;; Check rules for each gate
    (doseq [[in1 op in2 out] gates]
      (let [is-xy? #(or (str/starts-with? % "x") (str/starts-with? % "y"))
            is-xy-xor (and (is-xy? in1) (is-xy? in2))
            is-x00-y00 (or (and (= in1 "x00") (= in2 "y00"))
                           (and (= in1 "y00") (= in2 "x00")))
            is-z00 (and is-xy-xor is-x00-y00)]

        ;; Rule: XOR gates that don't take x,y as input should output to z
        (when (= op "XOR")
          (when (and (not is-xy-xor) (not (str/starts-with? out "z")))
            (swap! swapped conj out)))

        ;; Rule: z outputs (except last) should come from XOR
        (when (and (str/starts-with? out "z") (not= out max-z))
          (when (not= op "XOR")
            (swap! swapped conj out)))

        ;; Rule: AND gates (except x00 AND y00) should feed into OR
        (when (= op "AND")
          (when (not is-x00-y00)
            (let [used-by-or? (some (fn [[in1b op2 in2b _]]
                                      (and (= op2 "OR")
                                           (or (= out in1b) (= out in2b))))
                                    gates)]
              (when (not used-by-or?)
                (swap! swapped conj out)))))

        ;; Rule: XOR of x,y (except z00) should feed into XOR and AND
        (when (= op "XOR")
          (when (and is-xy-xor (not is-z00))
            (let [used-by-xor? (some (fn [[in1b op2 in2b _]]
                                       (and (= op2 "XOR")
                                            (or (= out in1b) (= out in2b))))
                                     gates)
                  used-by-and? (some (fn [[in1b op2 in2b _]]
                                       (and (= op2 "AND")
                                            (or (= out in1b) (= out in2b))))
                                     gates)]
              (when (not (and used-by-xor? used-by-and?))
                (swap! swapped conj out)))))))

    (str/join "," (sort @swapped))))

(defn main []
  (let [[wires gates] (parse-input "../input.txt")]
    (println "Part 1:" (part1 wires gates))
    (println "Part 2:" (part2 gates))))

(main)
