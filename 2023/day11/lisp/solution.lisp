#!/usr/bin/env sbcl --script
;;; Advent of Code 2023 Day 11: Cosmic Expansion
;;; Common Lisp Solution

(defun read-input (filename)
  "Read the input file and return a vector of lines."
  (with-open-file (stream filename :direction :input)
    (coerce (loop for line = (read-line stream nil nil)
                  while line
                  when (plusp (length line))
                  collect line)
            'vector)))

(defun parse-galaxies (lines)
  "Parse the grid and return a vector of galaxy positions as (row . col) pairs."
  (coerce (loop for row from 0
                for line across lines
                nconc (loop for col from 0 below (length line)
                            when (char= (char line col) #\#)
                            collect (cons row col)))
          'vector))

(defun find-empty-rows (lines)
  "Find row indices that contain no galaxies."
  (loop for row from 0
        for line across lines
        when (notany (lambda (ch) (char= ch #\#)) line)
        collect row))

(defun find-empty-cols (lines)
  "Find column indices that contain no galaxies."
  (when (plusp (length lines))
    (let ((num-cols (length (aref lines 0))))
      (loop for col from 0 below num-cols
            when (notany (lambda (line) (char= (char line col) #\#)) lines)
            collect col))))

(defun count-in-range (sorted-list min-val max-val)
  "Count how many values in sorted-list fall within [min-val, max-val)."
  (count-if (lambda (val) (and (>= val min-val) (< val max-val)))
            sorted-list))

(defun calculate-distance (g1 g2 empty-rows empty-cols expansion-factor)
  "Calculate the expanded Manhattan distance between two galaxies."
  (let* ((r1 (car g1))
         (c1 (cdr g1))
         (r2 (car g2))
         (c2 (cdr g2))
         (min-r (min r1 r2))
         (max-r (max r1 r2))
         (min-c (min c1 c2))
         (max-c (max c1 c2))
         (base-dist (+ (- max-r min-r) (- max-c min-c)))
         (empty-row-count (count-in-range empty-rows min-r max-r))
         (empty-col-count (count-in-range empty-cols min-c max-c)))
    (+ base-dist
       (* empty-row-count (1- expansion-factor))
       (* empty-col-count (1- expansion-factor)))))

(defun calculate-total-distances (galaxies empty-rows empty-cols expansion-factor)
  "Calculate sum of distances between all pairs of galaxies."
  (let ((n (length galaxies)))
    (loop for i from 0 below (1- n)
          sum (loop for j from (1+ i) below n
                    sum (calculate-distance (aref galaxies i)
                                            (aref galaxies j)
                                            empty-rows
                                            empty-cols
                                            expansion-factor)))))

(defun solve (lines expansion-factor)
  "Solve the problem with the given expansion factor."
  (let ((galaxies (parse-galaxies lines))
        (empty-rows (find-empty-rows lines))
        (empty-cols (find-empty-cols lines)))
    (calculate-total-distances galaxies empty-rows empty-cols expansion-factor)))

(defun part1 (lines)
  "Solve Part 1 - expansion factor of 2."
  (solve lines 2))

(defun part2 (lines)
  "Solve Part 2 - expansion factor of 1,000,000."
  (solve lines 1000000))

(defun main ()
  "Main entry point."
  (let* ((input-file (if (> (length sb-ext:*posix-argv*) 1)
                         (second sb-ext:*posix-argv*)
                         "../input.txt"))
         (lines (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 lines))
    (format t "Part 2: ~A~%" (part2 lines))))

(main)
