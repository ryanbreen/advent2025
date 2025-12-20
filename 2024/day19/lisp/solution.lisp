#!/usr/bin/env sbcl --script

;;; Day 19: Linen Layout
;;; Count designs that can be formed from towel patterns and count all ways

(defun split-by-comma (string)
  "Split a string by comma into a list of trimmed strings."
  (loop for start = 0 then (1+ end)
        for end = (position #\, string :start start)
        collect (string-trim '(#\Space #\Tab)
                             (subseq string start (or end (length string))))
        while end))

(defun read-input (filename)
  "Read input file, returning (patterns . designs)."
  (with-open-file (stream filename)
    (let* ((patterns-line (read-line stream nil))
           (patterns (split-by-comma patterns-line))
           (designs '()))
      ;; Skip blank line
      (read-line stream nil)
      ;; Read designs
      (loop for line = (read-line stream nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                 (when (> (length trimmed) 0)
                   (push trimmed designs))))
      (cons patterns (nreverse designs)))))

(defun string-prefix-p (prefix prefix-len string start string-len)
  "Check if STRING starting at START has PREFIX of length PREFIX-LEN."
  (let ((end (+ start prefix-len)))
    (and (<= end string-len)
         (not (mismatch prefix string :start2 start :end2 end)))))

(defun count-ways (design patterns-with-lengths)
  "Count the number of ways to form DESIGN from PATTERNS-WITH-LENGTHS using DP with memoization.
   PATTERNS-WITH-LENGTHS is a list of (pattern . length) pairs."
  (let ((memo (make-hash-table :test 'eql))
        (design-len (length design)))
    (labels ((dp (pos)
               (cond
                 ((= pos design-len) 1)
                 ((gethash pos memo))
                 (t
                  (let ((total 0))
                    (dolist (pl patterns-with-lengths)
                      (let ((pattern (car pl))
                            (plen (cdr pl)))
                        (when (string-prefix-p pattern plen design pos design-len)
                          (incf total (dp (+ pos plen))))))
                    (setf (gethash pos memo) total)
                    total)))))
      (dp 0))))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (input (read-input input-file))
         (patterns (car input))
         (designs (cdr input))
         ;; Pre-compute pattern lengths
         (patterns-with-lengths (mapcar (lambda (p) (cons p (length p))) patterns))
         ;; Compute count-ways once per design
         (counts (mapcar (lambda (d) (count-ways d patterns-with-lengths)) designs)))
    (format t "Part 1: ~A~%" (count-if #'plusp counts))
    (format t "Part 2: ~A~%" (reduce #'+ counts))))

(main)
