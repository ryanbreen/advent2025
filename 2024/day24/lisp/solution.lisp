#!/usr/bin/env sbcl --script

;;; Day 24: Crossed Wires - Logic gate simulation and adder circuit analysis

(defstruct gate
  in1
  op
  in2
  out)

(defun split-string (string separator)
  "Split a string by separator character."
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string) do
      (when (char= (char string i) separator)
        (push (subseq string start i) result)
        (setf start (1+ i))))
    (push (subseq string start) result)
    (nreverse result)))

(defun split-by-double-newline (string)
  "Split a string by double newline."
  (let ((result '())
        (start 0))
    (loop for i from 0 below (1- (length string)) do
      (when (and (char= (char string i) #\Newline)
                (char= (char string (1+ i)) #\Newline))
        (push (subseq string start i) result)
        (setf start (+ i 2))
        (return)))
    (push (subseq string start) result)
    (nreverse result)))

(defun parse-input (filename)
  "Parse initial wire values and gates from the input file."
  (with-open-file (stream filename)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (let* ((parts (split-by-double-newline content))
             (wires (make-hash-table :test 'equal))
             (gates '()))

        ;; Parse initial wire values
        (dolist (line (split-string (first parts) #\Newline))
          (when (> (length line) 0)
            (let* ((colon-pos (position #\: line))
                   (name (subseq line 0 colon-pos))
                   (val (parse-integer (string-trim " " (subseq line (+ colon-pos 1))))))
              (setf (gethash name wires) val))))

        ;; Parse gates
        (dolist (line (split-string (second parts) #\Newline))
          (when (> (length line) 0)
            (let ((parts (split-string line #\Space)))
              (push (make-gate :in1 (nth 0 parts)
                              :op (nth 1 parts)
                              :in2 (nth 2 parts)
                              :out (nth 4 parts))
                    gates))))

        (values wires (nreverse gates))))))

(defun hash-table-copy (ht)
  "Create a shallow copy of a hash table."
  (let ((new-ht (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (key value)
               (setf (gethash key new-ht) value))
             ht)
    new-ht))

(defun simulate (wires gates)
  "Simulate the circuit until all outputs are computed."
  (let ((wires (hash-table-copy wires))
        (remaining (copy-list gates)))

    (loop while remaining do
      (let ((made-progress nil)
            (new-remaining '()))

        (dolist (gate remaining)
          (let ((v1 (gethash (gate-in1 gate) wires))
                (v2 (gethash (gate-in2 gate) wires)))
            (if (and v1 v2)
                (progn
                  (setf (gethash (gate-out gate) wires)
                        (cond
                          ((string= (gate-op gate) "AND") (logand v1 v2))
                          ((string= (gate-op gate) "OR") (logior v1 v2))
                          ((string= (gate-op gate) "XOR") (logxor v1 v2))
                          (t (error "Unknown operation: ~A" (gate-op gate)))))
                  (setf made-progress t))
                (push gate new-remaining))))

        (setf remaining (nreverse new-remaining))
        (when (and (not made-progress) remaining)
          (error "Circuit stuck - missing inputs"))))

    wires))

(defun get-z-value (wires)
  "Extract the number from z wires."
  (let ((z-wires (sort (loop for key being the hash-keys of wires
                            when (and (>= (length key) 1)
                                     (char= (char key 0) #\z))
                            collect key)
                       #'string>)))
    (let ((result 0))
      (dolist (z z-wires)
        (setf result (logior (ash result 1) (gethash z wires))))
      result)))

(defun part1 (wires gates)
  "Simulate and get the z output."
  (let ((final-wires (simulate wires gates)))
    (get-z-value final-wires)))

(defun part2 (gates)
  "Find the 8 swapped wires in the adder circuit.

   A correct ripple-carry adder has this structure:
   - Bit 0: z00 = x00 XOR y00, carry0 = x00 AND y00
   - Bit i: z[i] = (x[i] XOR y[i]) XOR carry[i-1]
            carry[i] = (x[i] AND y[i]) OR ((x[i] XOR y[i]) AND carry[i-1])

   We check structural rules to find violations."
  (let ((swapped (make-hash-table :test 'equal))
        (gate-by-output (make-hash-table :test 'equal))
        (gate-by-inputs-op (make-hash-table :test 'equal)))

    ;; Build lookup tables
    (dolist (gate gates)
      (setf (gethash (gate-out gate) gate-by-output) gate)
      (let ((key (cons (sort (list (gate-in1 gate) (gate-in2 gate)) #'string<)
                      (gate-op gate))))
        (setf (gethash key gate-by-inputs-op) (gate-out gate))))

    ;; Find the highest bit number
    (let ((max-bit (loop for gate in gates
                        when (and (>= (length (gate-out gate)) 1)
                                 (char= (char (gate-out gate) 0) #\z))
                        maximize (parse-integer (subseq (gate-out gate) 1)))))

      ;; Check structural rules
      (dolist (gate gates)
        (let ((in1 (gate-in1 gate))
              (in2 (gate-in2 gate))
              (op (gate-op gate))
              (out (gate-out gate)))

          ;; Rule: XOR gates that don't take x,y as input should output to z
          (when (string= op "XOR")
            (let ((is-xy-xor (and (or (and (>= (length in1) 1) (char= (char in1 0) #\x))
                                     (and (>= (length in1) 1) (char= (char in1 0) #\y)))
                                 (or (and (>= (length in2) 1) (char= (char in2 0) #\x))
                                     (and (>= (length in2) 1) (char= (char in2 0) #\y))))))
              (when (not is-xy-xor)
                (when (not (and (>= (length out) 1) (char= (char out 0) #\z)))
                  (setf (gethash out swapped) t)))))

          ;; Rule: z outputs (except last) should come from XOR
          (when (and (>= (length out) 1)
                    (char= (char out 0) #\z)
                    (not (string= out (format nil "z~2,'0d" max-bit))))
            (when (not (string= op "XOR"))
              (setf (gethash out swapped) t)))

          ;; Rule: AND gates (except x00 AND y00) should feed into OR
          (when (string= op "AND")
            (let ((is-first-bit (and (or (string= in1 "x00") (string= in2 "x00"))
                                    (or (string= in1 "y00") (string= in2 "y00")))))
              (when (not is-first-bit)
                (let ((used-by-or nil))
                  (dolist (g2 gates)
                    (when (and (string= (gate-op g2) "OR")
                              (or (string= out (gate-in1 g2))
                                  (string= out (gate-in2 g2))))
                      (setf used-by-or t)
                      (return)))
                  (when (not used-by-or)
                    (setf (gethash out swapped) t))))))

          ;; Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
          (when (string= op "XOR")
            (let ((is-xy-xor (and (or (and (>= (length in1) 1) (char= (char in1 0) #\x))
                                     (and (>= (length in1) 1) (char= (char in1 0) #\y)))
                                 (or (and (>= (length in2) 1) (char= (char in2 0) #\x))
                                     (and (>= (length in2) 1) (char= (char in2 0) #\y)))))
                  (is-z00 (and (or (string= in1 "x00") (string= in2 "x00"))
                              (or (string= in1 "y00") (string= in2 "y00")))))
              (when (and is-xy-xor (not is-z00))
                (let ((used-by-xor nil)
                      (used-by-and nil))
                  (dolist (g2 gates)
                    (when (or (string= out (gate-in1 g2))
                             (string= out (gate-in2 g2)))
                      (cond
                        ((string= (gate-op g2) "XOR") (setf used-by-xor t))
                        ((string= (gate-op g2) "AND") (setf used-by-and t)))))
                  (when (not (and used-by-xor used-by-and))
                    (setf (gethash out swapped) t))))))))

      ;; Return sorted comma-separated list
      (let ((swapped-list (loop for key being the hash-keys of swapped
                               collect key)))
        (format nil "~{~A~^,~}" (sort swapped-list #'string<))))))

(defun main ()
  (multiple-value-bind (wires gates) (parse-input "../input.txt")
    (format t "Part 1: ~A~%" (part1 wires gates))
    (format t "Part 2: ~A~%" (part2 gates))))

(main)
