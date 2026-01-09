#!/usr/bin/env sbcl --script
;;;; Day 12: Hill Climbing Algorithm - BFS shortest path in a heightmap

;;; Two-list queue for amortized O(1) enqueue/dequeue
(defstruct (queue (:constructor make-queue ()))
  (front nil :type list)
  (back nil :type list))

(defun queue-empty-p (q)
  "Check if queue is empty."
  (and (null (queue-front q)) (null (queue-back q))))

(defun enqueue (q item)
  "Add item to back of queue."
  (push item (queue-back q)))

(defun dequeue (q)
  "Remove and return item from front of queue."
  (when (null (queue-front q))
    (setf (queue-front q) (nreverse (queue-back q))
          (queue-back q) nil))
  (pop (queue-front q)))

(defun parse-input (filename)
  "Parse the grid and find start (S) and end (E) positions.
   Returns: (values grid start end rows cols)
   where grid is a 2D array of heights (0-25), start and end are (row . col) cons cells."
  (let ((lines nil))
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            when (> (length line) 0)
            do (push line lines)))
    (setf lines (nreverse lines))

    (let* ((rows (length lines))
           (cols (length (first lines)))
           (grid (make-array (list rows cols) :element-type 'fixnum))
           (start nil)
           (end nil))

      ;; Parse the grid
      (loop for row from 0 below rows
            for line in lines
            do (loop for col from 0 below cols
                     for ch = (char line col)
                     do (cond
                          ((char= ch #\S)
                           (setf start (cons row col))
                           (setf (aref grid row col) 0))  ; 'a' = 0
                          ((char= ch #\E)
                           (setf end (cons row col))
                           (setf (aref grid row col) 25)) ; 'z' = 25
                          (t
                           (setf (aref grid row col) (- (char-code ch) (char-code #\a)))))))

      (values grid start end rows cols))))

(defun make-coord-key (row col cols)
  "Create a unique hash key for a coordinate pair."
  (+ (* row cols) col))

(defun bfs (grid starts end rows cols)
  "BFS to find shortest path from any start position to end.
   Movement constraint: destination height at most 1 higher than current.
   Returns the shortest distance, or -1 if no path exists."
  (let ((directions '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
        (visited (make-hash-table :test 'eql))
        (q (make-queue))
        (end-row (car end))
        (end-col (cdr end)))

    ;; Initialize with all start positions
    (dolist (start starts)
      (let* ((r (car start))
             (c (cdr start))
             (key (make-coord-key r c cols)))
        (unless (gethash key visited)
          (setf (gethash key visited) t)
          (enqueue q (list r c 0)))))

    ;; BFS loop
    (loop until (queue-empty-p q) do
      (destructuring-bind (row col dist) (dequeue q)
        ;; Check if we reached the end
        (when (and (= row end-row) (= col end-col))
          (return-from bfs dist))

        (let ((current-height (aref grid row col)))
          ;; Explore neighbors
          (dolist (dir directions)
            (let ((nrow (+ row (car dir)))
                  (ncol (+ col (cdr dir))))
              (when (and (>= nrow 0) (< nrow rows)
                         (>= ncol 0) (< ncol cols))
                (let ((key (make-coord-key nrow ncol cols))
                      (next-height (aref grid nrow ncol)))
                  ;; Can move if destination is at most 1 higher
                  (when (and (not (gethash key visited))
                             (<= next-height (1+ current-height)))
                    (setf (gethash key visited) t)
                    (enqueue q (list nrow ncol (1+ dist)))))))))))

    -1))  ; No path found

(defun part1 (grid start end rows cols)
  "Find shortest path from S to E."
  (bfs grid (list start) end rows cols))

(defun part2 (grid end rows cols)
  "Find shortest path from any 'a' (height 0) to E."
  (let ((starts nil))
    ;; Find all cells with elevation 'a' (height 0)
    (loop for row from 0 below rows
          do (loop for col from 0 below cols
                   when (= (aref grid row col) 0)
                   do (push (cons row col) starts)))
    (bfs grid starts end rows cols)))

(defun main ()
  (multiple-value-bind (grid start end rows cols)
      (parse-input "../input.txt")
    (format t "Part 1: ~D~%" (part1 grid start end rows cols))
    (format t "Part 2: ~D~%" (part2 grid end rows cols))))

(main)
