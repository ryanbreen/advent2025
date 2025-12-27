;;; Day 6: Wait For It - Common Lisp solution

(defun read-input ()
  "Read and return the input file contents."
  (with-open-file (stream "../input.txt")
    (let ((times-line (read-line stream))
          (distances-line (read-line stream)))
      (list times-line distances-line))))

(defun parse-numbers (line)
  "Extract numbers from a line like 'Time:   38   67   76   73'."
  (let ((colon-pos (position #\: line)))
    (with-input-from-string (s (subseq line (1+ colon-pos)))
      (loop for num = (read s nil nil)
            while num
            collect num))))

(defun parse-races (lines)
  "Parse input lines into list of (time . distance) pairs."
  (let ((times (parse-numbers (first lines)))
        (distances (parse-numbers (second lines))))
    (mapcar #'cons times distances)))

(defun count-ways-to-win (time record)
  "Count integer values t where t * (time - t) > record.
   Solve -t^2 + time*t - record > 0 using quadratic formula."
  (let* ((discriminant (- (* time time) (* 4 record))))
    (if (<= discriminant 0)
        0
        (let* ((sqrt-d (sqrt (coerce discriminant 'double-float)))
               (t-low (/ (- time sqrt-d) 2.0d0))
               (t-high (/ (+ time sqrt-d) 2.0d0))
               ;; We need integer values strictly between the roots
               (first-win (1+ (floor t-low)))
               (last-win (1- (ceiling t-high))))
          (if (< last-win first-win)
              0
              (1+ (- last-win first-win)))))))

(defun part1 (lines)
  "Calculate product of ways to win for all races."
  (let ((races (parse-races lines)))
    (reduce #'* (mapcar (lambda (race)
                          (count-ways-to-win (car race) (cdr race)))
                        races))))

(defun concatenate-numbers (numbers)
  "Concatenate a list of numbers into a single number."
  (parse-integer (format nil "~{~a~}" numbers)))

(defun part2 (lines)
  "Treat all times/distances as single concatenated numbers."
  (let* ((races (parse-races lines))
         (time (concatenate-numbers (mapcar #'car races)))
         (record (concatenate-numbers (mapcar #'cdr races))))
    (count-ways-to-win time record)))

(defun main ()
  (let ((lines (read-input)))
    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: ~a~%" (part2 lines))))

(main)
