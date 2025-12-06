#!/usr/bin/env -S sbcl --script

;;; Advent of Code 2024 - Day 6: Guard Gallivant
;;; Common Lisp Solution

(defun read-grid (filename)
  "Read the grid from file and return as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun find-guard (grid)
  "Find the starting position and direction of the guard.
   Returns (row col direction) where direction is one of ^, v, <, >"
  (loop for row-idx from 0
        for row in grid
        do (loop for col-idx from 0 below (length row)
                 for char = (char row col-idx)
                 when (member char '(#\^ #\v #\< #\>))
                 do (return-from find-guard (list row-idx col-idx char)))))

(defun turn-right (direction)
  "Turn 90 degrees to the right"
  (case direction
    (#\^ #\>)
    (#\> #\v)
    (#\v #\<)
    (#\< #\^)))

(defun get-next-pos (row col direction)
  "Get the next position based on current position and direction"
  (case direction
    (#\^ (list (1- row) col))
    (#\v (list (1+ row) col))
    (#\< (list row (1- col)))
    (#\> (list row (1+ col)))))

(defun in-bounds-p (row col height width)
  "Check if position is within the grid"
  (and (>= row 0) (< row height)
       (>= col 0) (< col width)))

(defun is-obstacle-p (grid row col)
  "Check if position contains an obstacle (#)"
  (char= (char (nth row grid) col) #\#))

(defun simulate-guard (grid)
  "Simulate the guard's movement and return count of distinct positions visited"
  (let* ((height (length grid))
         (width (length (first grid)))
         (start-info (find-guard grid))
         (row (first start-info))
         (col (second start-info))
         (direction (third start-info))
         (visited (make-hash-table :test 'equal)))

    ;; Mark starting position as visited
    (setf (gethash (list row col) visited) t)

    ;; Simulate guard movement
    (loop
      (let* ((next-pos (get-next-pos row col direction))
             (next-row (first next-pos))
             (next-col (second next-pos)))

        ;; Check if next position is out of bounds
        (unless (in-bounds-p next-row next-col height width)
          (return))

        ;; Check if there's an obstacle ahead
        (if (is-obstacle-p grid next-row next-col)
            ;; Turn right if obstacle ahead
            (setf direction (turn-right direction))
            ;; Otherwise, move forward
            (progn
              (setf row next-row)
              (setf col next-col)
              (setf (gethash (list row col) visited) t)))))

    ;; Return count of distinct positions
    (hash-table-count visited)))

(defun get-visited-positions (grid)
  "Get all positions visited during guard's patrol"
  (let* ((height (length grid))
         (width (length (first grid)))
         (start-info (find-guard grid))
         (row (first start-info))
         (col (second start-info))
         (direction (third start-info))
         (visited (make-hash-table :test 'equal)))

    ;; Mark starting position as visited
    (setf (gethash (list row col) visited) t)

    ;; Simulate guard movement
    (loop
      (let* ((next-pos (get-next-pos row col direction))
             (next-row (first next-pos))
             (next-col (second next-pos)))

        ;; Check if next position is out of bounds
        (unless (in-bounds-p next-row next-col height width)
          (return))

        ;; Check if there's an obstacle ahead
        (if (is-obstacle-p grid next-row next-col)
            ;; Turn right if obstacle ahead
            (setf direction (turn-right direction))
            ;; Otherwise, move forward
            (progn
              (setf row next-row)
              (setf col next-col)
              (setf (gethash (list row col) visited) t)))))

    ;; Return list of visited positions
    (loop for key being the hash-keys of visited
          collect key)))

(defun detect-loop-p (grid)
  "Check if the guard gets stuck in a loop with the current grid configuration"
  (let* ((height (length grid))
         (width (length (first grid)))
         (start-info (find-guard grid))
         (row (first start-info))
         (col (second start-info))
         (direction (third start-info))
         (states (make-hash-table :test 'equal)))

    ;; Track states (position + direction)
    (loop
      (let ((state (list row col direction)))
        ;; If we've seen this state before, we're in a loop
        (when (gethash state states)
          (return-from detect-loop-p t))
        (setf (gethash state states) t))

      (let* ((next-pos (get-next-pos row col direction))
             (next-row (first next-pos))
             (next-col (second next-pos)))

        ;; Check if next position is out of bounds
        (unless (in-bounds-p next-row next-col height width)
          (return-from detect-loop-p nil))

        ;; Check if there's an obstacle ahead
        (if (is-obstacle-p grid next-row next-col)
            ;; Turn right if obstacle ahead
            (setf direction (turn-right direction))
            ;; Otherwise, move forward
            (progn
              (setf row next-row)
              (setf col next-col)))))))

(defun copy-grid (grid)
  "Create a copy of the grid for modification"
  (mapcar (lambda (row) (copy-seq row)) grid))

(defun set-obstacle (grid row col)
  "Set a position as an obstacle in the grid"
  (let ((row-str (nth row grid)))
    (setf (nth row grid)
          (concatenate 'string
                      (subseq row-str 0 col)
                      "#"
                      (subseq row-str (1+ col))))))

(defun count-loop-positions (grid)
  "Count positions where adding an obstruction creates a loop"
  (let* ((visited-positions (get-visited-positions grid))
         (start-info (find-guard grid))
         (start-row (first start-info))
         (start-col (second start-info))
         (loop-count 0))

    ;; Try placing an obstruction at each visited position
    (dolist (pos visited-positions)
      (let ((test-row (first pos))
            (test-col (second pos)))

        ;; Skip the starting position
        (unless (and (= test-row start-row) (= test-col start-col))
          ;; Create a modified grid with the obstruction
          (let ((modified-grid (copy-grid grid)))
            (set-obstacle modified-grid test-row test-col)

            ;; Check if this creates a loop
            (when (detect-loop-p modified-grid)
              (incf loop-count))))))

    loop-count))

(defun part1 ()
  "Solve part 1: Count distinct positions visited by guard"
  (let ((grid (read-grid "../input.txt")))
    (simulate-guard grid)))

(defun part2 ()
  "Solve part 2: Count positions where adding an obstruction creates a loop"
  (let ((grid (read-grid "../input.txt")))
    (count-loop-positions grid)))

(defun main ()
  "Main entry point"
  (format t "Part 1: ~a~%" (part1))
  (format t "Part 2: ~a~%" (part2)))

;; Run the solution
(main)
