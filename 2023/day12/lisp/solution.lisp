;;;; Advent of Code 2023 Day 12: Hot Springs
;;;; Common Lisp solution

(defun split-string (string separator)
  "Split STRING by SEPARATOR character, returning a list of substrings."
  (loop with len = (length string)
        with start = 0
        for end = (position separator string :start start)
        collect (subseq string start (or end len))
        while end
        do (setf start (1+ end))))

(defun parse-line (line)
  "Parse a line into (pattern . groups) where groups is a list of integers."
  (let* ((space-pos (position #\Space line))
         (pattern (subseq line 0 space-pos))
         (groups-str (subseq line (1+ space-pos)))
         (groups (mapcar #'parse-integer
                         (remove-if (lambda (s) (string= s ""))
                                    (split-string groups-str #\,)))))
    (cons pattern groups)))

(defun count-arrangements (pattern groups)
  "Count valid arrangements using memoized DP."
  (let ((memo (make-hash-table :test 'equal))
        (pattern-len (length pattern))
        (groups-vec (coerce groups 'vector))
        (num-groups (length groups)))
    (labels ((dp (pos group-idx current-run)
               "DP: position in pattern, current group index, length of current run."
               (let ((key (list pos group-idx current-run)))
                 (multiple-value-bind (cached found) (gethash key memo)
                   (if found
                       cached
                       (setf (gethash key memo)
                             (compute-dp pos group-idx current-run))))))

             (compute-dp (pos group-idx current-run)
               "Compute DP value for given state."
               ;; Base case: reached end of pattern
               (if (= pos pattern-len)
                   (cond
                     ;; Valid if all groups matched and no partial run
                     ((and (= group-idx num-groups) (= current-run 0)) 1)
                     ;; Or if on last group and run matches
                     ((and (= group-idx (1- num-groups))
                           (= (aref groups-vec group-idx) current-run)) 1)
                     (t 0))
                   ;; Not at end
                   (let ((char (char pattern pos))
                         (result 0))
                     ;; Option 1: Place operational spring (.)
                     (when (or (char= char #\.) (char= char #\?))
                       (if (= current-run 0)
                           ;; No active run, just move forward
                           (incf result (dp (1+ pos) group-idx 0))
                           ;; End current run if it matches expected group size
                           (when (and (< group-idx num-groups)
                                      (= (aref groups-vec group-idx) current-run))
                             (incf result (dp (1+ pos) (1+ group-idx) 0)))))

                     ;; Option 2: Place damaged spring (#)
                     (when (or (char= char #\#) (char= char #\?))
                       (when (and (< group-idx num-groups)
                                  (< current-run (aref groups-vec group-idx)))
                         (incf result (dp (1+ pos) group-idx (1+ current-run)))))

                     result))))

      (dp 0 0 0))))

(defun unfold (pattern groups &optional (times 5))
  "Unfold pattern and groups by repeating them 'times' times."
  (let ((unfolded-pattern (format nil "~{~A~^?~}" (loop repeat times collect pattern)))
        (unfolded-groups (loop repeat times append groups)))
    (cons unfolded-pattern unfolded-groups)))

(defun read-input (filename)
  "Read input file and return list of non-empty lines."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) line)) 0)
            collect line)))

(defun part1 (lines)
  "Sum of arrangement counts for all rows."
  (loop for line in lines
        for (pattern . groups) = (parse-line line)
        sum (count-arrangements pattern groups)))

(defun part2 (lines)
  "Sum of arrangement counts for all rows after unfolding."
  (loop for line in lines
        for (pattern . groups) = (parse-line line)
        for (unfolded-pattern . unfolded-groups) = (unfold pattern groups)
        sum (count-arrangements unfolded-pattern unfolded-groups)))

(defun main ()
  (let ((lines (read-input "../input.txt")))
    (format t "Part 1: ~A~%" (part1 lines))
    (format t "Part 2: ~A~%" (part2 lines))))

(main)
