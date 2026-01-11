#!/usr/bin/env sbcl --script
;;;; Day 5: Hydrothermal Venture

(defun sign (x)
  "Return -1, 0, or 1 based on the sign of x."
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (t 0)))

(defun parse-line (line)
  "Parse a line like '0,9 -> 5,9' into (x1 y1 x2 y2)."
  (let* ((arrow-pos (search " -> " line))
         (start (subseq line 0 arrow-pos))
         (end (subseq line (+ arrow-pos 4)))
         (comma1 (position #\, start))
         (comma2 (position #\, end)))
    (list (parse-integer (subseq start 0 comma1))
          (parse-integer (subseq start (1+ comma1)))
          (parse-integer (subseq end 0 comma2))
          (parse-integer (subseq end (1+ comma2))))))

(defun read-input ()
  "Read and parse input file."
  (let ((input-path (merge-pathnames "../input.txt" *load-truename*)))
    (with-open-file (stream input-path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            when (> (length line) 0)
            collect (parse-line line)))))

(defun make-point-key (x y)
  "Create a unique key for a point using Cantor pairing (handles negatives)."
  (let ((px (if (>= x 0) (* 2 x) (1- (- (* 2 x)))))
        (py (if (>= y 0) (* 2 y) (1- (- (* 2 y))))))
    (+ (/ (* (+ px py) (+ px py 1)) 2) py)))

(defun count-overlaps (lines include-diagonals)
  "Count points where at least two lines overlap."
  (let ((grid (make-hash-table :test 'eql)))
    (dolist (line lines)
      (destructuring-bind (x1 y1 x2 y2) line
        (let ((dx (sign (- x2 x1)))
              (dy (sign (- y2 y1))))
          ;; Skip diagonals if not including them
          (unless (and (not include-diagonals)
                       (/= dx 0)
                       (/= dy 0))
            (loop with x = x1
                  with y = y1
                  do (let ((key (make-point-key x y)))
                       (incf (gethash key grid 0)))
                     (when (and (= x x2) (= y y2))
                       (return))
                     (incf x dx)
                     (incf y dy))))))
    ;; Count points with value >= 2
    (loop for v being the hash-values of grid
          count (>= v 2))))

(defun part1 (lines)
  "Solve part 1: only horizontal and vertical lines."
  (count-overlaps lines nil))

(defun part2 (lines)
  "Solve part 2: include diagonal lines."
  (count-overlaps lines t))

(defun main ()
  (let ((lines (read-input)))
    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: ~a~%" (part2 lines))))

(main)
