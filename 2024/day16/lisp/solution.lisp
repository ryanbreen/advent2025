#!/usr/bin/env -S sbcl --script
;;;; Day 16: Reindeer Maze - Weighted shortest path with turn costs

(defstruct state
  (cost 0 :type integer)
  (x 0 :type integer)
  (y 0 :type integer)
  (dir 0 :type integer))

(defun state< (a b)
  "Priority queue comparison: lower cost first."
  (< (state-cost a) (state-cost b)))

;;; Priority queue implementation using a binary heap
(defstruct pq
  (heap (make-array 16 :adjustable t :fill-pointer 0))
  (size 0 :type integer))

(defun pq-push (pq item)
  "Add item to priority queue."
  (vector-push-extend item (pq-heap pq))
  (incf (pq-size pq))
  ;; Bubble up
  (loop with idx = (1- (fill-pointer (pq-heap pq)))
        while (> idx 0)
        do (let ((parent (ash (1- idx) -1)))
             (if (state< (aref (pq-heap pq) idx)
                        (aref (pq-heap pq) parent))
                 (progn
                   (rotatef (aref (pq-heap pq) idx)
                           (aref (pq-heap pq) parent))
                   (setf idx parent))
                 (return)))))

(defun pq-pop (pq)
  "Remove and return minimum item from priority queue."
  (when (zerop (pq-size pq))
    (return-from pq-pop nil))

  (let ((result (aref (pq-heap pq) 0))
        (last-idx (1- (fill-pointer (pq-heap pq)))))
    (setf (aref (pq-heap pq) 0) (aref (pq-heap pq) last-idx))
    (decf (fill-pointer (pq-heap pq)))
    (decf (pq-size pq))

    ;; Bubble down
    (when (> (pq-size pq) 0)
      (loop with idx = 0
            for left = (+ (* 2 idx) 1)
            while (< left (fill-pointer (pq-heap pq)))
            do (let* ((right (1+ left))
                      (smallest idx))
                 (when (state< (aref (pq-heap pq) left)
                              (aref (pq-heap pq) smallest))
                   (setf smallest left))
                 (when (and (< right (fill-pointer (pq-heap pq)))
                           (state< (aref (pq-heap pq) right)
                                  (aref (pq-heap pq) smallest)))
                   (setf smallest right))
                 (if (= smallest idx)
                     (return)
                     (progn
                       (rotatef (aref (pq-heap pq) idx)
                               (aref (pq-heap pq) smallest))
                       (setf idx smallest))))))
    result))

(defun pq-empty-p (pq)
  "Check if priority queue is empty."
  (zerop (pq-size pq)))

;;; Direction vectors: 0=East, 1=South, 2=West, 3=North
(defparameter *dx* #(1 0 -1 0))
(defparameter *dy* #(0 1 0 -1))

(defun parse-input (filename)
  "Parse maze and return grid, start, and end positions."
  (let ((grid '())
        (start nil)
        (end nil))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
            for y from 0
            while line
            do (let ((row (coerce line 'vector)))
                 (push row grid)
                 (loop for x from 0 below (length row)
                       do (case (aref row x)
                            (#\S (setf start (cons x y)))
                            (#\E (setf end (cons x y))))))))
    (values (coerce (nreverse grid) 'vector) start end)))

(defun make-state-key (x y dir)
  "Create a unique key for state (x, y, dir)."
  ;; Assuming grid is < 256x256, pack into single integer
  (+ (* x 1048576) (* y 4096) dir))

(defun dijkstra-forward (grid start)
  "Run Dijkstra from start facing East. Returns hash table of state -> cost."
  (let ((pq (make-pq))
        (dist (make-hash-table :test 'eql))
        (height (length grid))
        (width (length (aref grid 0))))

    ;; Start facing East (direction 0)
    (pq-push pq (make-state :cost 0
                           :x (car start)
                           :y (cdr start)
                           :dir 0))

    (loop until (pq-empty-p pq)
          for current = (pq-pop pq)
          for state-key = (make-state-key (state-x current)
                                         (state-y current)
                                         (state-dir current))
          unless (gethash state-key dist)
          do (progn
               (setf (gethash state-key dist) (state-cost current))

               ;; Move forward
               (let* ((x (state-x current))
                      (y (state-y current))
                      (d (state-dir current))
                      (nx (+ x (aref *dx* d)))
                      (ny (+ y (aref *dy* d))))
                 (when (and (>= nx 0) (< nx width)
                           (>= ny 0) (< ny height)
                           (char/= (aref (aref grid ny) nx) #\#))
                   (pq-push pq (make-state :cost (1+ (state-cost current))
                                          :x nx :y ny :dir d))))

               ;; Turn left
               (pq-push pq (make-state :cost (+ (state-cost current) 1000)
                                      :x (state-x current)
                                      :y (state-y current)
                                      :dir (mod (1- (state-dir current)) 4)))

               ;; Turn right
               (pq-push pq (make-state :cost (+ (state-cost current) 1000)
                                      :x (state-x current)
                                      :y (state-y current)
                                      :dir (mod (1+ (state-dir current)) 4)))))
    dist))

(defun dijkstra-backward (grid end)
  "Run Dijkstra backward from end (all directions). Returns hash table."
  (let ((pq (make-pq))
        (dist (make-hash-table :test 'eql))
        (height (length grid))
        (width (length (aref grid 0))))

    ;; At end, we can arrive from any direction
    (loop for d from 0 below 4
          do (pq-push pq (make-state :cost 0
                                    :x (car end)
                                    :y (cdr end)
                                    :dir d)))

    (loop until (pq-empty-p pq)
          for current = (pq-pop pq)
          for state-key = (make-state-key (state-x current)
                                         (state-y current)
                                         (state-dir current))
          unless (gethash state-key dist)
          do (progn
               (setf (gethash state-key dist) (state-cost current))

               ;; Reverse of move forward: come from behind
               (let* ((x (state-x current))
                      (y (state-y current))
                      (d (state-dir current))
                      (px (- x (aref *dx* d)))
                      (py (- y (aref *dy* d))))
                 (when (and (>= px 0) (< px width)
                           (>= py 0) (< py height)
                           (char/= (aref (aref grid py) px) #\#))
                   (pq-push pq (make-state :cost (1+ (state-cost current))
                                          :x px :y py :dir d))))

               ;; Reverse of turn: same position, different direction
               (pq-push pq (make-state :cost (+ (state-cost current) 1000)
                                      :x (state-x current)
                                      :y (state-y current)
                                      :dir (mod (1- (state-dir current)) 4)))

               (pq-push pq (make-state :cost (+ (state-cost current) 1000)
                                      :x (state-x current)
                                      :y (state-y current)
                                      :dir (mod (1+ (state-dir current)) 4)))))
    dist))

(defun part1 (grid start end)
  "Find the lowest score path from start to end."
  (let ((dist (dijkstra-forward grid start)))
    (loop for d from 0 below 4
          for state-key = (make-state-key (car end) (cdr end) d)
          minimize (gethash state-key dist most-positive-fixnum))))

(defun part2 (grid start end best-score)
  "Count tiles that are part of any optimal path."
  (let ((dist-from-start (dijkstra-forward grid start))
        (dist-to-end (dijkstra-backward grid end))
        (tiles (make-hash-table :test 'equal))
        (height (length grid))
        (width (length (aref grid 0))))

    (loop for y from 0 below height
          do (loop for x from 0 below width
                   when (char/= (aref (aref grid y) x) #\#)
                   do (loop for d from 0 below 4
                           for state-key = (make-state-key x y d)
                           for from-start = (gethash state-key dist-from-start
                                                    most-positive-fixnum)
                           for to-end = (gethash state-key dist-to-end
                                                most-positive-fixnum)
                           when (= (+ from-start to-end) best-score)
                           do (progn
                                (setf (gethash (cons x y) tiles) t)
                                (return)))))

    (hash-table-count tiles)))

(defun main ()
  (multiple-value-bind (grid start end)
      (parse-input "../input.txt")

    (let ((answer1 (part1 grid start end)))
      (format t "Part 1: ~A~%" answer1)

      (let ((answer2 (part2 grid start end answer1)))
        (format t "Part 2: ~A~%" answer2)))))

(main)
