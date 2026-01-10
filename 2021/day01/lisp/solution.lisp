;;;; Day 1: Sonar Sweep - Advent of Code 2021

(defun read-depths (filename)
  "Read depth values from file, returning a vector for O(1) access."
  (with-open-file (stream filename :direction :input)
    (coerce (loop for line = (read-line stream nil nil)
                  while line
                  collect (parse-integer line))
            'vector)))

(defun part1 (depths)
  "Count the number of times a depth measurement increases from the previous."
  (loop for i from 1 below (length depths)
        count (> (aref depths i) (aref depths (1- i)))))

(defun part2 (depths)
  "Count increases in 3-measurement sliding window sums.
   Optimization: comparing window[i] > window[i-1] is equivalent to
   comparing depths[i+2] > depths[i-1] since the middle two elements cancel."
  (loop for i from 3 below (length depths)
        count (> (aref depths i) (aref depths (- i 3)))))

(defun main ()
  (let ((depths (read-depths "../input.txt")))
    (format t "Part 1: ~a~%" (part1 depths))
    (format t "Part 2: ~a~%" (part2 depths))))

(main)
