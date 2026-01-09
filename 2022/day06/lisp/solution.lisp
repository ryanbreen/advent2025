#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 - Day 6: Tuning Trouble

(defun find-marker (data window-size)
  "Find first position where last WINDOW-SIZE characters are all unique."
  (loop for i from window-size to (length data)
        for window = (subseq data (- i window-size) i)
        when (= (length (remove-duplicates window)) window-size)
          return i))

(defun part1 (data)
  "Find the start-of-packet marker (4 unique chars)."
  (find-marker data 4))

(defun part2 (data)
  "Find the start-of-message marker (14 unique chars)."
  (find-marker data 14))

(defun main ()
  (let* ((script-path (or *load-truename* *default-pathname-defaults*))
         (input-path (merge-pathnames "../input.txt" script-path))
         (data (string-trim '(#\Newline #\Return #\Space)
                           (with-open-file (stream input-path)
                             (let ((contents (make-string (file-length stream))))
                               (read-sequence contents stream)
                               contents)))))
    (format t "Part 1: ~A~%" (part1 data))
    (format t "Part 2: ~A~%" (part2 data))))

(main)
