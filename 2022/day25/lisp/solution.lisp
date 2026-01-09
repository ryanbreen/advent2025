#!/usr/bin/env sbcl --script

(defun snafu-digit-value (char)
  "Convert a SNAFU digit character to its decimal value."
  (case char
    (#\2 2)
    (#\1 1)
    (#\0 0)
    (#\- -1)
    (#\= -2)))

(defun snafu-to-decimal (s)
  "Convert a SNAFU number string to decimal."
  (let ((result 0))
    (loop for char across s do
      (setf result (+ (* result 5) (snafu-digit-value char))))
    result))

(defun decimal-to-snafu (n)
  "Convert a decimal number to SNAFU."
  (if (zerop n)
      "0"
      (let ((digits '()))
        (loop while (not (zerop n)) do
          (let ((remainder (mod n 5)))
            (cond
              ((<= remainder 2)
               (push (digit-char remainder) digits)
               (setf n (floor n 5)))
              ((= remainder 3)
               (push #\= digits)
               (setf n (+ (floor n 5) 1)))
              (t ; remainder == 4
               (push #\- digits)
               (setf n (+ (floor n 5) 1))))))
        (coerce digits 'string))))

(defun read-lines (filename)
  "Read all lines from a file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun part1 (lines)
  "Sum all SNAFU numbers and return result as SNAFU."
  (let ((total (reduce #'+ (mapcar #'snafu-to-decimal lines))))
    (decimal-to-snafu total)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (lines (read-lines input-file)))
    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: No Part 2 on Day 25!~%")))

(main)
