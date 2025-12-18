#!/usr/bin/env sbcl --script

(defconstant +grid-size+ 71)
(defconstant +num-bytes+ 1024)

(defun parse-input (filename)
  "Parse byte positions from input file. Returns a list of (x . y) cons cells."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
          collect (let* ((comma-pos (position #\, line))
                         (x (parse-integer (subseq line 0 comma-pos)))
                         (y (parse-integer (subseq line (1+ comma-pos)))))
                    (cons x y)))))

(defun make-coord-key (x y)
  "Create a unique hash key for a coordinate pair."
  (+ (* y +grid-size+) x))

(defun bfs (corrupted-set size)
  "Find shortest path from (0,0) to (size-1, size-1) using BFS.
   Returns -1 if no path exists."
  (let ((start-key (make-coord-key 0 0))
        (goal-x (1- size))
        (goal-y (1- size))
        (directions '((0 . 1) (0 . -1) (1 . 0) (-1 . 0)))
        (visited (make-hash-table :test 'eql))
        (queue nil))

    ;; Check if start or goal is corrupted
    (when (or (gethash start-key corrupted-set)
              (gethash (make-coord-key goal-x goal-y) corrupted-set))
      (return-from bfs -1))

    ;; Initialize BFS
    (setf (gethash start-key visited) t)
    (push (list 0 0 0) queue)  ; (x y steps)
    (setf queue (nreverse queue))

    (loop while queue do
      (let* ((current (pop queue))
             (x (first current))
             (y (second current))
             (steps (third current)))

        ;; Check if we reached the goal
        (when (and (= x goal-x) (= y goal-y))
          (return-from bfs steps))

        ;; Explore neighbors
        (dolist (dir directions)
          (let ((nx (+ x (car dir)))
                (ny (+ y (cdr dir))))
            (when (and (>= nx 0) (< nx size)
                       (>= ny 0) (< ny size))
              (let ((key (make-coord-key nx ny)))
                (unless (or (gethash key visited)
                            (gethash key corrupted-set))
                  (setf (gethash key visited) t)
                  (setf queue (nconc queue (list (list nx ny (1+ steps))))))))))))

    -1))

(defun build-corrupted-set (positions count)
  "Build a hash set of corrupted positions from the first COUNT positions."
  (let ((corrupted (make-hash-table :test 'eql)))
    (loop for i from 0 below (min count (length positions))
          for pos in positions
          do (setf (gethash (make-coord-key (car pos) (cdr pos)) corrupted) t))
    corrupted))

(defun part1 (positions &optional (num-bytes +num-bytes+) (size +grid-size+))
  "Find shortest path after first NUM-BYTES have fallen."
  (let ((corrupted (build-corrupted-set positions num-bytes)))
    (bfs corrupted size)))

(defun part2 (positions &optional (size +grid-size+))
  "Find the first byte that blocks all paths using binary search."
  (let ((left 0)
        (right (length positions)))
    (loop while (< left right) do
      (let* ((mid (floor (+ left right) 2))
             (corrupted (build-corrupted-set positions (1+ mid)))
             (result (bfs corrupted size)))
        (if (= result -1)
            (setf right mid)
            (setf left (1+ mid)))))
    (let ((blocking-pos (nth left positions)))
      (format nil "~D,~D" (car blocking-pos) (cdr blocking-pos)))))

(defun main ()
  (let ((positions (parse-input "../input.txt")))
    (format t "Part 1: ~D~%" (part1 positions))
    (format t "Part 2: ~A~%" (part2 positions))))

(main)
