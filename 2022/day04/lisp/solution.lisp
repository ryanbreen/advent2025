#!/usr/bin/env sbcl --script
;;; Day 4: Camp Cleanup - Range overlap detection

(defun parse-range (str)
  "Parse a range string like '2-4' into (start . end)."
  (let ((dash-pos (position #\- str)))
    (cons (parse-integer (subseq str 0 dash-pos))
          (parse-integer (subseq str (1+ dash-pos))))))

(defun parse-line (line)
  "Parse a line like '2-4,6-8' into ((a1 . b1) (a2 . b2))."
  (let ((comma-pos (position #\, line)))
    (list (parse-range (subseq line 0 comma-pos))
          (parse-range (subseq line (1+ comma-pos))))))

(defun read-input (filename)
  "Read and parse all lines from input file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
          collect (parse-line line))))

(defun fully-contains-p (pair)
  "Check if one range fully contains the other."
  (destructuring-bind ((a1 . b1) (a2 . b2)) pair
    (or (and (<= a1 a2) (>= b1 b2))
        (and (<= a2 a1) (>= b2 b1)))))

(defun overlaps-p (pair)
  "Check if ranges overlap at all."
  (destructuring-bind ((a1 . b1) (a2 . b2)) pair
    (and (<= a1 b2) (<= a2 b1))))

(defun part1 (pairs)
  "Count pairs where one range fully contains the other."
  (count-if #'fully-contains-p pairs))

(defun part2 (pairs)
  "Count pairs where ranges overlap at all."
  (count-if #'overlaps-p pairs))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (pairs (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 pairs))
    (format t "Part 2: ~A~%" (part2 pairs))))

(main)
