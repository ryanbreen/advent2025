#!/usr/bin/env sbcl --script

(defun read-input (filename)
  "Read input file and return a list of strings (grid rows)"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun get-char (grid row col)
  "Get character at (row, col) in grid, or NIL if out of bounds"
  (when (and (>= row 0) (< row (length grid))
             (>= col 0) (< col (length (nth row grid))))
    (char (nth row grid) col)))

(defun check-word-from-position (grid row col dr dc target)
  "Check if TARGET word exists starting from (row, col) in direction (dr, dc)"
  (loop for i from 0 below (length target)
        for r = (+ row (* dr i))
        for c = (+ col (* dc i))
        for ch = (get-char grid r c)
        always (and ch (char= ch (char target i)))))

(defun part1 (grid)
  "Count all occurrences of XMAS in all 8 directions"
  (let ((target "XMAS")
        (rows (length grid))
        (cols (length (first grid)))
        (directions '((0 1)   ; right
                      (0 -1)  ; left
                      (1 0)   ; down
                      (-1 0)  ; up
                      (1 1)   ; down-right
                      (1 -1)  ; down-left
                      (-1 1)  ; up-right
                      (-1 -1) ; up-left
                      ))
        (count 0))
    (loop for r from 0 below rows do
      (loop for c from 0 below cols do
        (loop for dir in directions
              for dr = (first dir)
              for dc = (second dir)
              when (check-word-from-position grid r c dr dc target)
                do (incf count))))
    count))

(defun check-x-mas (grid row col)
  "Check if an X-MAS pattern exists centered at (row, col)"
  (when (char= (get-char grid row col) #\A)
    (let ((top-left (get-char grid (1- row) (1- col)))
          (top-right (get-char grid (1- row) (1+ col)))
          (bottom-left (get-char grid (1+ row) (1- col)))
          (bottom-right (get-char grid (1+ row) (1+ col))))
      ;; Check if all corners exist
      (when (and top-left top-right bottom-left bottom-right)
        ;; Check diagonal 1 (top-left to bottom-right): MAS or SAM
        (let ((diag1-ok (or (and (char= top-left #\M) (char= bottom-right #\S))
                            (and (char= top-left #\S) (char= bottom-right #\M))))
              ;; Check diagonal 2 (top-right to bottom-left): MAS or SAM
              (diag2-ok (or (and (char= top-right #\M) (char= bottom-left #\S))
                            (and (char= top-right #\S) (char= bottom-left #\M)))))
          (and diag1-ok diag2-ok))))))

(defun part2 (grid)
  "Count all X-MAS patterns (two MAS forming an X)"
  (let ((rows (length grid))
        (cols (length (first grid)))
        (count 0))
    ;; Check each possible center point (skip borders since we need all 4 corners)
    (loop for r from 1 below (1- rows) do
      (loop for c from 1 below (1- cols) do
        (when (check-x-mas grid r c)
          (incf count))))
    count))

(defun main ()
  "Main entry point"
  (let* ((input-path (merge-pathnames "../input.txt" *load-truename*))
         (grid (read-input input-path)))
    (format t "Part 1: ~a~%" (part1 grid))
    (format t "Part 2: ~a~%" (part2 grid))))

(main)
