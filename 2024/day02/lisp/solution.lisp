#!/usr/bin/env sbcl --script

(defun parse-input (filename)
  "Parse input file into list of reports (each report is a list of numbers)"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (with-input-from-string (s line)
                    (loop for num = (read s nil nil)
                          while num
                          collect num)))))

(defun all-increasing-p (levels)
  "Check if all adjacent pairs are increasing by 1-3"
  (loop for (a b) on levels
        while b
        always (and (< a b) (<= (- b a) 3))))

(defun all-decreasing-p (levels)
  "Check if all adjacent pairs are decreasing by 1-3"
  (loop for (a b) on levels
        while b
        always (and (> a b) (<= (- a b) 3))))

(defun is-safe-p (levels)
  "Check if a report is safe"
  (or (all-increasing-p levels)
      (all-decreasing-p levels)))

(defun remove-at (list index)
  "Remove element at given index from list"
  (append (subseq list 0 index)
          (subseq list (1+ index))))

(defun is-safe-with-dampener-p (levels)
  "Check if a report is safe, or can be made safe by removing one level"
  (if (is-safe-p levels)
      t
      ;; Try removing each level one at a time
      (loop for i from 0 below (length levels)
            thereis (is-safe-p (remove-at levels i)))))

(defun part1 (reports)
  "Count safe reports"
  (count-if #'is-safe-p reports))

(defun part2 (reports)
  "Count reports that are safe or can be made safe with Problem Dampener"
  (count-if #'is-safe-with-dampener-p reports))

(defun main ()
  (let ((reports (parse-input "../input.txt")))
    (format t "Part 1: ~a~%" (part1 reports))
    (format t "Part 2: ~a~%" (part2 reports))))

(main)
