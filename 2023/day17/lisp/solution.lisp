#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 17 - Clumsy Crucible
;;; Uses Dijkstra's shortest path with movement constraints
;;; State: (row, col, direction, consecutive_steps)

(defconstant +right+ 0)
(defconstant +down+ 1)
(defconstant +left+ 2)
(defconstant +up+ 3)

(defparameter *dr* #(0 1 0 -1))
(defparameter *dc* #(1 0 -1 0))

;;; Simple binary heap priority queue implementation
(defstruct (priority-queue (:conc-name pq-))
  (heap (make-array 16 :adjustable t :fill-pointer 0)))

(defun pq-empty-p (pq)
  "Check if priority queue is empty."
  (zerop (fill-pointer (pq-heap pq))))

(defun pq-push (pq priority item)
  "Push an item with given priority (lower = higher priority)."
  (let ((heap (pq-heap pq))
        (entry (cons priority item)))
    (vector-push-extend entry heap)
    ;; Bubble up
    (loop with idx = (1- (fill-pointer heap))
          while (> idx 0)
          for parent = (floor (1- idx) 2)
          while (< (car entry) (car (aref heap parent)))
          do (setf (aref heap idx) (aref heap parent)
                   (aref heap parent) entry
                   idx parent))))

(defun pq-pop (pq)
  "Pop and return the minimum priority item. Returns (priority . item)."
  (let* ((heap (pq-heap pq))
         (result (aref heap 0))
         (last-idx (1- (fill-pointer heap))))
    (when (> last-idx 0)
      (setf (aref heap 0) (aref heap last-idx)))
    (decf (fill-pointer heap))
    ;; Bubble down
    (when (> (fill-pointer heap) 1)
      (loop with idx = 0
            with len = (fill-pointer heap)
            for left = (1+ (* 2 idx))
            for right = (+ 2 (* 2 idx))
            for smallest = idx
            do (when (and (< left len)
                          (< (car (aref heap left)) (car (aref heap smallest))))
                 (setf smallest left))
               (when (and (< right len)
                          (< (car (aref heap right)) (car (aref heap smallest))))
                 (setf smallest right))
               (if (= smallest idx)
                   (return)
                   (let ((tmp (aref heap idx)))
                     (setf (aref heap idx) (aref heap smallest)
                           (aref heap smallest) tmp
                           idx smallest)))))
    result))

(defun read-grid (filename)
  "Read the input file and return a 2D array of integers."
  (with-open-file (stream filename :direction :input)
    (let ((lines nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (push line lines))
      (let* ((lines-vec (coerce (nreverse lines) 'vector))
             (rows (length lines-vec))
             (cols (length (aref lines-vec 0)))
             (grid (make-array (list rows cols) :element-type 'fixnum)))
        (dotimes (r rows)
          (dotimes (c cols)
            (setf (aref grid r c)
                  (- (char-code (char (aref lines-vec r) c)) (char-code #\0)))))
        grid))))

(defun make-state-key (r c d consec)
  "Create a unique key for a state (row, col, direction, consecutive)."
  ;; Assumes max grid size ~200x200, directions 0-3, consec 0-10
  (+ (* r 100000) (* c 100) (* d 11) consec))

(defun dijkstra (grid min-straight max-straight)
  "Find minimum heat loss path using Dijkstra's algorithm.
   State: (row, col, direction, consecutive_steps)
   Directions: 0=right, 1=down, 2=left, 3=up"
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (visited (make-hash-table :test 'eql))
         (pq (make-priority-queue)))

    ;; Start: heat=0, position=(0,0), direction=-1 (none yet), consecutive=0
    (pq-push pq 0 (list 0 0 -1 0))

    (loop while (not (pq-empty-p pq)) do
      (let* ((entry (pq-pop pq))
             (heat (car entry))
             (state (cdr entry))
             (r (first state))
             (c (second state))
             (d (third state))
             (consec (fourth state)))

        ;; Check if we reached the goal
        (when (and (= r (1- rows)) (= c (1- cols)))
          (when (or (zerop min-straight) (>= consec min-straight))
            (return-from dijkstra heat)))

        (let ((state-key (make-state-key r c d consec)))
          (unless (gethash state-key visited)
            (setf (gethash state-key visited) t)

            ;; Try all four directions
            (dotimes (nd 4)
              ;; Can't reverse direction
              (unless (and (/= d -1) (= nd (mod (+ d 2) 4)))
                (let ((nr (+ r (aref *dr* nd)))
                      (nc (+ c (aref *dc* nd))))

                  ;; Bounds check
                  (when (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))
                    (let ((new-consec
                            (if (= nd d)
                                ;; Continuing in same direction
                                (1+ consec)
                                ;; Turning
                                1)))

                      ;; Check consecutive constraints
                      (when (and (<= new-consec max-straight)
                                 (or (= nd d)
                                     (= d -1)
                                     (>= consec min-straight)))
                        (let* ((new-heat (+ heat (aref grid nr nc)))
                               (new-state-key (make-state-key nr nc nd new-consec)))

                          (unless (gethash new-state-key visited)
                            (pq-push pq new-heat (list nr nc nd new-consec))))))))))))))

    -1)) ; No path found

(defun part1 (grid)
  "Part 1: Normal crucible, max 3 consecutive blocks."
  (dijkstra grid 0 3))

(defun part2 (grid)
  "Part 2: Ultra crucible, min 4 and max 10 consecutive blocks."
  (dijkstra grid 4 10))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (grid (read-grid input-file)))
    (format t "Part 1: ~A~%" (part1 grid))
    (format t "Part 2: ~A~%" (part2 grid))))

(main)
