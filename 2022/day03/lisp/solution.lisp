#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 - Day 3: Rucksack Reorganization

(defun read-input (filename)
  "Read lines from file, filtering empty lines."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
            collect line)))

(defun char-priority (char)
  "Calculate priority: a-z=1-26, A-Z=27-52."
  (let ((code (char-code char)))
    (if (lower-case-p char)
        (1+ (- code (char-code #\a)))
        (+ 27 (- code (char-code #\A))))))

(defun string-to-set (str)
  "Convert a string to a list of unique characters (as a set)."
  (remove-duplicates (coerce str 'list)))

(defun set-intersection-all (sets)
  "Find intersection of all sets (lists of characters)."
  (reduce (lambda (a b) (intersection a b)) sets))

(defun part1 (rucksacks)
  "Find sum of priorities of common items between compartments."
  (loop for rucksack in rucksacks
        for mid = (/ (length rucksack) 2)
        for first-half = (subseq rucksack 0 mid)
        for second-half = (subseq rucksack mid)
        for first-set = (string-to-set first-half)
        for second-set = (string-to-set second-half)
        for common = (intersection first-set second-set)
        sum (char-priority (first common))))

(defun part2 (rucksacks)
  "Find sum of priorities of badge items for each group of 3."
  (loop for i from 0 below (length rucksacks) by 3
        for group = (subseq rucksacks i (+ i 3))
        for sets = (mapcar #'string-to-set group)
        for common = (set-intersection-all sets)
        sum (char-priority (first common))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (rucksacks (read-input input-file)))
    (format t "Part 1: ~a~%" (part1 rucksacks))
    (format t "Part 2: ~a~%" (part2 rucksacks))))

(main)
