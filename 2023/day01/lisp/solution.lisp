#!/usr/bin/env sbcl --script

(defparameter *words*
  '(("one" . 1) ("two" . 2) ("three" . 3) ("four" . 4) ("five" . 5)
    ("six" . 6) ("seven" . 7) ("eight" . 8) ("nine" . 9)))

(defun read-input ()
  "Read the input file and return a list of lines"
  (let ((input-path (merge-pathnames "../input.txt"
                                     (directory-namestring *load-pathname*))))
    (with-open-file (stream input-path)
      (loop for line = (read-line stream nil)
            while line
            collect line))))

(defun part1 (lines)
  "Part 1: Extract first and last digit from each line"
  (loop for line in lines
        for digits = (loop for char across line
                          for digit = (digit-char-p char)
                          when digit
                          collect digit)
        when digits
        sum (+ (* (first digits) 10) (first (last digits)))))

(defun find-digit-at (line pos)
  "Find digit at position - either numeric or spelled word"
  (let ((char (char line pos)))
    (cond
      ((digit-char-p char) (digit-char-p char))
      (t (loop for (word . value) in *words*
               when (and (<= (+ pos (length word)) (length line))
                        (string= line word :start1 pos :end1 (+ pos (length word))))
               return value)))))

(defun part2 (lines)
  "Part 2: Extract digits including spelled-out words"
  (loop for line in lines
        for digits = (loop for i from 0 below (length line)
                          for digit = (find-digit-at line i)
                          when digit collect digit)
        when digits
        sum (+ (* (first digits) 10) (first (last digits)))))

(defun main ()
  "Main entry point"
  (let ((lines (read-input)))
    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: ~a~%" (part2 lines))))

(main)
