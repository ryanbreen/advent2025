#!/usr/bin/env sbcl --script

;;; Day 3: Lobby - Common Lisp Solution
;;; Part 1: Select 2 batteries per bank, maximize 2-digit number
;;; Part 2: Select 12 batteries per bank, maximize 12-digit number

(defun read-input-file (filename)
  "Read the input file and return a list of lines."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun char-to-digit (char)
  "Convert a character digit to an integer."
  (- (char-code char) (char-code #\0)))

(defun part1 (lines)
  "Solve Part 1: Select 2 batteries to maximize 2-digit joltage."
  (let ((total 0))
    (dolist (line lines)
      (let* ((n (length line))
             ;; Precompute max suffix: max_suffix[i] = max digit from position i to end
             (max-suffix (make-array n :initial-element 0)))

        ;; Build max-suffix array from right to left
        (setf (aref max-suffix (1- n)) (char-to-digit (char line (1- n))))
        (loop for i from (- n 2) downto 0 do
          (setf (aref max-suffix i)
                (max (char-to-digit (char line i))
                     (aref max-suffix (1+ i)))))

        ;; Find maximum joltage for this bank
        (let ((max-joltage 0))
          (loop for i from 0 below (1- n) do
            (let* ((first-digit (char-to-digit (char line i)))
                   (max-second (aref max-suffix (1+ i)))
                   (joltage (+ (* first-digit 10) max-second)))
              (setf max-joltage (max max-joltage joltage))))

          (incf total max-joltage))))
    total))

(defun part2 (lines)
  "Solve Part 2: Select 12 batteries to maximize 12-digit joltage."
  (let ((total 0))
    (dolist (line lines)
      (let* ((n (length line))
             (k 12) ; Select exactly 12 batteries
             (result '())
             (current-pos 0))

        ;; Greedy algorithm to select k digits that form the maximum number
        (loop for i from 0 below k do
          (let* ((remaining-needed (- k i 1))
                 (search-end (- n remaining-needed))
                 (max-digit -1)
                 (max-pos current-pos))

            ;; Find the maximum digit in the valid range
            (loop for j from current-pos below search-end do
              (let ((digit (char-to-digit (char line j))))
                (when (> digit max-digit)
                  (setf max-digit digit)
                  (setf max-pos j))))

            (push max-digit result)
            (setf current-pos (1+ max-pos))))

        ;; Convert result list to number
        (let ((joltage 0))
          (dolist (digit (nreverse result))
            (setf joltage (+ (* joltage 10) digit)))
          (incf total joltage))))
    total))

(defun main ()
  "Main entry point."
  (let* ((script-path *load-truename*)
         (day-dir (make-pathname :directory (butlast (pathname-directory script-path))))
         (input-file (merge-pathnames "input.txt" day-dir))
         (lines (read-input-file input-file)))

    (format t "Part 1: ~A~%" (part1 lines))
    (format t "Part 2: ~A~%" (part2 lines))))

;; Run the main function
(main)
