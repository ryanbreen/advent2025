#!/usr/bin/env sbcl --script
;;;; Day 10: Factory - Button press minimization

(defun count-bits (n)
  "Count number of 1 bits in integer n."
  (logcount n))

(defun split-string (string separator)
  "Split string by separator character."
  (loop for start = 0 then (1+ end)
        for end = (position separator string :start start)
        collect (subseq string start end)
        while end))

(defun parse-line (line)
  "Parse machine line into target state and button masks for Part 1."
  (let ((indicator-start (position #\[ line))
        (indicator-end (position #\] line))
        (n-lights 0)
        (target 0)
        (buttons nil))

    ;; Parse indicator pattern
    (when (and indicator-start indicator-end)
      (let ((indicator (subseq line (1+ indicator-start) indicator-end)))
        (setf n-lights (length indicator))
        (loop for i from 0 below n-lights
              for c = (char indicator i)
              when (char= c #\#)
              do (setf target (logior target (ash 1 i))))))

    ;; Parse button schematics
    (let ((pos 0))
      (loop while (setf pos (position #\( line :start pos))
            do (let ((end-pos (position #\) line :start pos)))
                 (when end-pos
                   (let* ((button-str (subseq line (1+ pos) end-pos))
                          (indices (mapcar #'parse-integer
                                          (split-string button-str #\,)))
                          (mask 0))
                     (dolist (idx indices)
                       (setf mask (logior mask (ash 1 idx))))
                     (push mask buttons)
                     (setf pos (1+ end-pos)))))))

    (values n-lights target (nreverse buttons))))

(defun solve-machine-brute (n-lights target buttons)
  "Brute force: try all combinations of button presses."
  (let ((n-buttons (length buttons))
        (min-presses most-positive-fixnum))

    (loop for mask from 0 below (ash 1 n-buttons)
          do (let ((state 0)
                   (presses 0))
               (loop for i from 0 below n-buttons
                     when (logbitp i mask)
                     do (setf state (logxor state (nth i buttons)))
                        (incf presses))
               (when (= state target)
                 (setf min-presses (min min-presses presses)))))

    (if (= min-presses most-positive-fixnum) 0 min-presses)))

(defun part1 (lines)
  "Find minimum total button presses for all machines."
  (let ((total 0))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) line)))
        (unless (zerop (length trimmed))
          (multiple-value-bind (n-lights target buttons)
              (parse-line trimmed)
            (incf total (solve-machine-brute n-lights target buttons))))))
    total))

;;; Part 2 - Gaussian Elimination with Rational Arithmetic

(defun parse-line-part2 (line)
  "Parse machine line for Part 2."
  (let ((joltage nil)
        (buttons nil))

    ;; Extract joltage requirements
    (let ((jolt-start (position #\{ line))
          (jolt-end (position #\} line)))
      (when (and jolt-start jolt-end)
        (let ((jolt-str (subseq line (1+ jolt-start) jolt-end)))
          (setf joltage (mapcar #'parse-integer
                               (split-string jolt-str #\,))))))

    ;; Extract button schematics
    (let ((pos 0))
      (loop while (setf pos (position #\( line :start pos))
            do (let ((end-pos (position #\) line :start pos)))
                 (when end-pos
                   (let* ((button-str (subseq line (1+ pos) end-pos))
                          (indices (mapcar #'parse-integer
                                          (split-string button-str #\,))))
                     (push indices buttons)
                     (setf pos (1+ end-pos)))))))

    (values (length joltage) joltage (nreverse buttons))))

(defun make-augmented-matrix (n-counters joltage buttons)
  "Build augmented matrix [A | b]."
  (let* ((n-buttons (length buttons))
         (aug (make-array (list n-counters (1+ n-buttons))
                         :initial-element 0)))

    ;; Fill matrix A
    (loop for j from 0 below n-buttons
          for indices in buttons
          do (dolist (idx indices)
               (when (< idx n-counters)
                 (setf (aref aug idx j) 1))))

    ;; Fill vector b
    (loop for i from 0 below n-counters
          do (setf (aref aug i n-buttons) (nth i joltage)))

    aug))

(defun gaussian-elimination (aug n-rows n-cols)
  "Gaussian elimination to RREF. Returns pivot columns."
  (let ((pivot-cols nil)
        (pivot-row 0))

    (loop for col from 0 below n-cols
          do (let ((found -1))
               (loop for row from pivot-row below n-rows
                     when (not (zerop (aref aug row col)))
                     do (setf found row) (return))

               (when (>= found 0)
                 (unless (= found pivot-row)
                   (loop for c from 0 to n-cols
                         do (rotatef (aref aug pivot-row c)
                                    (aref aug found c))))

                 (push (cons col pivot-row) pivot-cols)

                 (let ((scale (aref aug pivot-row col)))
                   (loop for c from 0 to n-cols
                         do (setf (aref aug pivot-row c)
                                 (/ (aref aug pivot-row c) scale))))

                 (loop for row from 0 below n-rows
                       unless (= row pivot-row)
                       do (let ((factor (aref aug row col)))
                            (unless (zerop factor)
                              (loop for c from 0 to n-cols
                                    do (decf (aref aug row c)
                                            (* factor (aref aug pivot-row c)))))))

                 (incf pivot-row))))

    (nreverse pivot-cols)))

(defun solve-machine-part2 (n-counters joltage buttons)
  "Solve Part 2 using Gaussian elimination."
  (let ((n-buttons (length buttons)))

    (when (zerop n-buttons)
      (return-from solve-machine-part2
        (if (every #'zerop joltage) 0 most-positive-fixnum)))

    (let* ((aug (make-augmented-matrix n-counters joltage buttons))
           (n-cols n-buttons)
           (pivot-cols (gaussian-elimination aug n-counters n-cols)))

      ;; Check for inconsistency
      (let ((pivot-row (length pivot-cols)))
        (loop for row from pivot-row below n-counters
              when (not (zerop (aref aug row n-cols)))
              do (return-from solve-machine-part2 most-positive-fixnum)))

      ;; Identify free variables
      (let* ((pivot-col-set (mapcar #'car pivot-cols))
             (free-vars (loop for c from 0 below n-cols
                             unless (member c pivot-col-set)
                             collect c)))

        ;; No free variables - unique solution
        (when (null free-vars)
          (let ((total 0))
            (dolist (pc pivot-cols)
              (let ((val (aref aug (cdr pc) n-cols)))
                (unless (and (>= val 0) (integerp val))
                  (return-from solve-machine-part2 most-positive-fixnum))
                (incf total val)))
            (return-from solve-machine-part2 total)))

        ;; With free variables - simplified search for one free variable
        (when (= (length free-vars) 1)
          (let ((particular (make-array n-buttons :initial-element 0))
                (null-vec (make-array n-buttons :initial-element 0))
                (fv (first free-vars)))

            (setf (aref null-vec fv) 1)
            (dolist (pc pivot-cols)
              (let ((col (car pc))
                    (row (cdr pc)))
                (setf (aref particular col) (aref aug row n-cols))
                (setf (aref null-vec col) (- (aref aug row fv)))))

            (let ((t-low most-negative-fixnum)
                  (t-high most-positive-fixnum)
                  (min-total most-positive-fixnum))

              (dotimes (j n-buttons)
                (let ((p (aref particular j))
                      (nv (aref null-vec j)))
                  (cond
                    ((zerop nv)
                     (when (< p 0)
                       (return-from solve-machine-part2 most-positive-fixnum)))
                    ((> nv 0)
                     (setf t-low (max t-low (/ (- p) nv))))
                    (t
                     (setf t-high (min t-high (/ (- p) nv)))))))

              (when (> t-low t-high)
                (return-from solve-machine-part2 most-positive-fixnum))

              (loop for tt from (ceiling t-low) to (floor t-high)
                    do (let ((total 0)
                             (valid-solution t))
                         (dotimes (j n-buttons)
                           (let ((val (+ (aref particular j)
                                        (* tt (aref null-vec j)))))
                             (unless (and (>= val 0) (integerp val))
                               (setf valid-solution nil)
                               (return))
                             (incf total val)))
                         (when valid-solution
                           (setf min-total (min min-total total)))))

              (return-from solve-machine-part2
                (if (= min-total most-positive-fixnum) 0 min-total)))))

        ;; Two or more free variables
        (let ((n-free (length free-vars))
              (max-j (reduce #'max joltage :initial-value 100)))

          (when (<= n-free 6)
            ;; Extract null space vectors and particular solution
            (let ((null-vectors nil)
                  (particular (make-array n-buttons :initial-element 0)))

              (dolist (fv free-vars)
                (let ((vec (make-array n-buttons :initial-element 0)))
                  (setf (aref vec fv) 1)
                  (dolist (pc pivot-cols)
                    (setf (aref vec (car pc))
                          (- (aref aug (cdr pc) fv))))
                  (push vec null-vectors)))

              (setf null-vectors (nreverse null-vectors))

              (dolist (pc pivot-cols)
                (setf (aref particular (car pc))
                      (aref aug (cdr pc) n-cols)))

              ;; Specialized 2D search
              (when (= n-free 2)
                (let ((min-total most-positive-fixnum)
                      (nv0 (first null-vectors))
                      (nv1 (second null-vectors)))

                  ;; Compute initial bounds for t0
                  (let ((t0-low most-negative-fixnum)
                        (t0-high most-positive-fixnum))
                    (dotimes (j n-buttons)
                      (let ((p (aref particular j))
                            (nv (aref nv0 j)))
                        (cond
                          ((> nv 0)
                           (setf t0-low (max t0-low (/ (- p) nv))))
                          ((< nv 0)
                           (setf t0-high (min t0-high (/ (- p) nv)))))))

                    ;; Expand bounds
                    (setf t0-low (max (* max-j -2) (floor (- t0-low max-j))))
                    (setf t0-high (min (* max-j 2) (ceiling (+ t0-high max-j))))

                    ;; 2D search
                    (loop for t0 from t0-low to t0-high
                          do (let ((intermediate (make-array n-buttons)))
                               ;; Compute intermediate = particular + t0 * nv0
                               (dotimes (j n-buttons)
                                 (setf (aref intermediate j)
                                       (+ (aref particular j) (* t0 (aref nv0 j)))))

                               ;; Compute bounds for t1 given t0
                               (let ((t1-low most-negative-fixnum)
                                     (t1-high most-positive-fixnum))
                                 (dotimes (j n-buttons)
                                   (let ((p (aref intermediate j))
                                         (nv (aref nv1 j)))
                                     (cond
                                       ((> nv 0)
                                        (setf t1-low (max t1-low (/ (- p) nv))))
                                       ((< nv 0)
                                        (setf t1-high (min t1-high (/ (- p) nv)))))))

                                 (when (<= t1-low t1-high)
                                   (loop for t1 from (ceiling t1-low) to (floor t1-high)
                                         do (let ((valid-p t)
                                                  (total 0))
                                              (dotimes (j n-buttons)
                                                (let ((val (+ (aref intermediate j)
                                                             (* t1 (aref nv1 j)))))
                                                  (unless (and (>= val 0) (integerp val))
                                                    (setf valid-p nil)
                                                    (return))
                                                  (incf total val)))
                                              (when valid-p
                                                (setf min-total (min min-total total))))))))))

                  (return-from solve-machine-part2
                    (if (= min-total most-positive-fixnum) 0 min-total))))

              ;; Specialized 3D search
              (when (= n-free 3)
                (let ((min-total most-positive-fixnum)
                      (nv0 (first null-vectors))
                      (nv1 (second null-vectors))
                      (nv2 (third null-vectors))
                      (bound max-j))

                  (loop for t0 from (- bound) to bound
                        do (let ((inter0 (make-array n-buttons)))
                             ;; Compute inter0 = particular + t0 * nv0
                             (dotimes (j n-buttons)
                               (setf (aref inter0 j)
                                     (+ (aref particular j) (* t0 (aref nv0 j)))))

                             ;; Compute bounds for t1 given t0
                             (let ((t1-low most-negative-fixnum)
                                   (t1-high most-positive-fixnum))
                               (dotimes (j n-buttons)
                                 (let ((p (aref inter0 j))
                                       (nv (aref nv1 j)))
                                   (cond
                                     ((> nv 0)
                                      (setf t1-low (max t1-low (- (/ (- p) nv) bound))))
                                     ((< nv 0)
                                      (setf t1-high (min t1-high (+ (/ (- p) nv) bound)))))))

                               (setf t1-low (max (ceiling t1-low) (- bound)))
                               (setf t1-high (min (floor t1-high) bound))

                               (loop for t1 from t1-low to t1-high
                                     do (let ((inter1 (make-array n-buttons)))
                                          ;; Compute inter1 = inter0 + t1 * nv1
                                          (dotimes (j n-buttons)
                                            (setf (aref inter1 j)
                                                  (+ (aref inter0 j) (* t1 (aref nv1 j)))))

                                          ;; Compute bounds for t2 given t0, t1
                                          (let ((t2-low most-negative-fixnum)
                                                (t2-high most-positive-fixnum))
                                            (dotimes (j n-buttons)
                                              (let ((p (aref inter1 j))
                                                    (nv (aref nv2 j)))
                                                (cond
                                                  ((> nv 0)
                                                   (setf t2-low (max t2-low (/ (- p) nv))))
                                                  ((< nv 0)
                                                   (setf t2-high (min t2-high (/ (- p) nv)))))))

                                            (when (<= t2-low t2-high)
                                              (loop for t2 from (ceiling t2-low) to (floor t2-high)
                                                    do (let ((valid-p t)
                                                             (total 0))
                                                         (dotimes (j n-buttons)
                                                           (let ((val (+ (aref inter1 j)
                                                                        (* t2 (aref nv2 j)))))
                                                             (unless (and (>= val 0) (integerp val))
                                                               (setf valid-p nil)
                                                               (return))
                                                             (incf total val)))
                                                         (when valid-p
                                                           (setf min-total (min min-total total))))))))))))

                  (return-from solve-machine-part2
                    (if (= min-total most-positive-fixnum) 0 min-total))))

              ;; Search for optimal solution
              (let ((min-total most-positive-fixnum)
                    (bound (* max-j 2)))

                (labels ((search-combination (idx partial-sol)
                           (if (= idx n-free)
                               ;; Evaluate this combination
                               (let ((total 0)
                                     (valid-p t))
                                 (dotimes (j n-buttons)
                                   (let ((val (aref partial-sol j)))
                                     (unless (and (>= val 0) (integerp val))
                                       (setf valid-p nil)
                                       (return))
                                     (incf total val)))
                                 (when valid-p
                                   (setf min-total (min min-total total))))

                               ;; Try values for this free variable
                               (let ((t-low most-negative-fixnum)
                                     (t-high most-positive-fixnum))

                                 (dotimes (j n-buttons)
                                   (let ((p (aref partial-sol j))
                                         (nv (aref (nth idx null-vectors) j)))
                                     (cond
                                       ((> nv 0)
                                        (setf t-low (max t-low (/ (- p) nv))))
                                       ((< nv 0)
                                        (setf t-high (min t-high (/ (- p) nv)))))))

                                 (when (<= t-low t-high)
                                   ;; Widen bounds to account for integrality constraints
                                   ;; Python: max(ceil(t_low) - max_j, -max_j * 2)
                                   (let ((t-low-int (max (- (ceiling t-low) max-j) (- bound)))
                                         (t-high-int (min (+ (floor t-high) max-j) bound)))
                                     (loop for tt from t-low-int to t-high-int
                                           do (let ((new-partial (make-array n-buttons)))
                                                (dotimes (j n-buttons)
                                                  (setf (aref new-partial j)
                                                        (+ (aref partial-sol j)
                                                           (* tt (aref (nth idx null-vectors) j)))))
                                                (search-combination (1+ idx) new-partial)))))))))

                  (search-combination 0 (copy-seq particular)))

                (return-from solve-machine-part2
                  (if (= min-total most-positive-fixnum) 0 min-total)))))

          ;; Too many free variables - fallback
          0)))))

(defun part2 (lines)
  "Find minimum total button presses for joltage configuration."
  (let ((total 0))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) line)))
        (unless (zerop (length trimmed))
          (multiple-value-bind (n-counters joltage buttons)
              (parse-line-part2 trimmed)
            (incf total (solve-machine-part2 n-counters joltage buttons))))))
    total))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt"
                                      (or *load-truename* *default-pathname-defaults*)))
         (lines (with-open-file (stream input-file)
                  (loop for line = (read-line stream nil)
                        while line
                        collect line))))

    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: ~a~%" (part2 lines))))

(main)
