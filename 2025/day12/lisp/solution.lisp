#!/usr/bin/env sbcl --script
;;;; Day 12: Christmas Tree Farm - Polyomino Packing
;;;;
;;;; The solution checks if presents (polyominoes) can fit into rectangular regions.
;;;; For this problem, the constraint is simply: total cells needed <= available cells.

(defun count-hash-chars (lines)
  "Count the number of '#' characters in a list of lines."
  (loop for line in lines
        sum (count #\# line)))

(defun split-string (string)
  "Split string on whitespace."
  (let ((tokens '())
        (start nil))
    (loop for i from 0 below (length string)
          for char = (char string i)
          when (member char '(#\Space #\Tab))
          do (when start
               (push (subseq string start i) tokens)
               (setf start nil))
          else do (unless start
                    (setf start i)))
    ;; Handle final token
    (when start
      (push (subseq string start) tokens))
    (nreverse tokens)))

(defun process-section (section shapes regions)
  "Process a section as either a shape or region definition."
  (let* ((lines (reverse section))
         (first-line (first lines)))
    (if (and (find #\: first-line)
             (not (find #\x first-line)))
        ;; Shape definition
        (let* ((idx (parse-integer first-line :junk-allowed t))
               (shape-lines (rest lines))
               (cell-count (count-hash-chars shape-lines)))
          (setf (gethash idx shapes) cell-count))
        ;; Region definitions
        (dolist (region-line lines)
          (when (find #\x region-line)
            (let* ((colon-pos (position #\: region-line))
                   (dims (subseq region-line 0 colon-pos))
                   (x-pos (position #\x dims))
                   (width (parse-integer (subseq dims 0 x-pos)))
                   (height (parse-integer (subseq dims (1+ x-pos))))
                   (counts-str (string-trim '(#\Space) (subseq region-line (1+ colon-pos))))
                   (counts (mapcar #'parse-integer (split-string counts-str))))
              (push (list width height counts) regions))))))
  regions)

(defun parse-input (filename)
  "Parse input file into shapes and regions."
  (with-open-file (stream filename)
    (let ((shapes (make-hash-table))
          (regions '())
          (current-section '()))

      ;; Read all lines and split into sections
      (loop for line = (read-line stream nil)
            while line
            do (if (string= line "")
                   (when current-section
                     (setf regions (process-section current-section shapes regions))
                     (setf current-section '()))
                   (push line current-section)))

      ;; Process final section if exists
      (when current-section
        (setf regions (process-section current-section shapes regions)))

      (values shapes (reverse regions)))))

(defun can-fit-region (width height counts shapes)
  "Check if all presents can fit in the region."
  (let ((available (* width height))
        (total-cells-needed
          (loop for count in counts
                for i from 0
                sum (* count (gethash i shapes)))))
    (<= total-cells-needed available)))

(defun part1 (shapes regions)
  "Count regions that can fit all their presents."
  (loop for (width height counts) in regions
        count (can-fit-region width height counts shapes)))

(defun part2 ()
  "Part 2 is just a button click to finish - no computation needed."
  0)

(defun main ()
  (multiple-value-bind (shapes regions)
      (parse-input "../input.txt")
    (format t "Part 1: ~A~%" (part1 shapes regions))
    (format t "Part 2: ~A~%" (part2))))

(main)
