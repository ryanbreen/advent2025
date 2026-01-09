#!/usr/bin/env sbcl --script

(defun read-input (filename)
  "Read the grid from input file."
  (with-open-file (stream filename :direction :input)
    (let ((lines nil))
      (loop for line = (read-line stream nil nil)
            while line
            when (> (length line) 0)
            do (push line lines))
      (nreverse lines))))

(defun parse-grid (lines)
  "Parse list of strings into 2D array of integers."
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols) :element-type 'fixnum)))
    (loop for row from 0 below rows
          for line in lines
          do (loop for col from 0 below cols
                   do (setf (aref grid row col)
                            (- (char-code (char line col)) (char-code #\0)))))
    grid))

(defun is-visible (grid row col)
  "Check if tree at (row, col) is visible from any direction."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (height (aref grid row col)))
    ;; Check from left
    (or (loop for c from 0 below col
              always (< (aref grid row c) height))
        ;; Check from right
        (loop for c from (1+ col) below cols
              always (< (aref grid row c) height))
        ;; Check from top
        (loop for r from 0 below row
              always (< (aref grid r col) height))
        ;; Check from bottom
        (loop for r from (1+ row) below rows
              always (< (aref grid r col) height)))))

(defun viewing-distance (grid row col dr dc)
  "Count trees visible in direction (dr, dc) from (row, col)."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (height (aref grid row col))
        (count 0)
        (r (+ row dr))
        (c (+ col dc)))
    (loop while (and (>= r 0) (< r rows) (>= c 0) (< c cols))
          do (incf count)
             (when (>= (aref grid r c) height)
               (return))
             (incf r dr)
             (incf c dc))
    count))

(defun scenic-score (grid row col)
  "Calculate scenic score for tree at (row, col)."
  (* (viewing-distance grid row col 0 -1)   ; left
     (viewing-distance grid row col 0 1)    ; right
     (viewing-distance grid row col -1 0)   ; up
     (viewing-distance grid row col 1 0)))  ; down

(defun part1 (grid)
  "Count trees visible from outside the grid."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (count 0))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (is-visible grid r c)
                   do (incf count)))
    count))

(defun part2 (grid)
  "Find maximum scenic score."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (max-score 0))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   do (setf max-score (max max-score (scenic-score grid r c)))))
    max-score))

(defun main ()
  (let* ((script-path (or *load-pathname* *compile-file-pathname*
                          (make-pathname :directory '(:relative))))
         (input-file (merge-pathnames "../input.txt" script-path))
         (lines (read-input input-file))
         (grid (parse-grid lines)))
    (format t "Part 1: ~a~%" (part1 grid))
    (format t "Part 2: ~a~%" (part2 grid))))

(main)
