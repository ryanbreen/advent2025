#!/usr/bin/env sbcl --script

(defun parse-input (filename)
  "Parse input file into two lists of numbers"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          for parts = (with-input-from-string (s line)
                        (list (read s) (read s)))
          collect (first parts) into left
          collect (second parts) into right
          finally (return (values left right)))))

(defun part1 (left right)
  "Sort both lists and calculate total distance between paired elements"
  (let ((sorted-left (sort (copy-list left) #'<))
        (sorted-right (sort (copy-list right) #'<)))
    (reduce #'+ (mapcar (lambda (l r) (abs (- l r))) sorted-left sorted-right))))

(defun count-occurrences (num list)
  "Count how many times num appears in list"
  (count num list))

(defun part2 (left right)
  "Calculate similarity score: sum of (left number * count in right list)"
  (reduce #'+ (mapcar (lambda (num) (* num (count-occurrences num right))) left)))

(defun main ()
  (multiple-value-bind (left right) (parse-input "../input.txt")
    (format t "Part 1: ~a~%" (part1 left right))
    (format t "Part 2: ~a~%" (part2 left right))))

(main)
