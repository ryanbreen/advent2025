#!/usr/bin/env sbcl --script
;;;; Day 21: Step Counter - Garden plot reachability

(defun read-grid (filename)
  "Read grid from file and return grid as vector of strings plus start position."
  (let ((grid (make-array 0 :adjustable t :fill-pointer 0))
        (start-row nil)
        (start-col nil))
    (with-open-file (in filename :direction :input)
      (loop for line = (read-line in nil nil)
            for row from 0
            while line
            do (vector-push-extend line grid)
               (let ((col (position #\S line)))
                 (when col
                   (setf start-row row)
                   (setf start-col col)))))
    (values grid start-row start-col)))

(defun grid-ref (grid row col)
  "Get character at grid position, or nil if out of bounds."
  (let ((rows (length grid))
        (cols (length (aref grid 0))))
    (if (and (>= row 0) (< row rows) (>= col 0) (< col cols))
        (char (aref grid row) col)
        nil)))

(defun grid-ref-infinite (grid row col)
  "Get character at grid position on infinite tiled grid."
  (let* ((rows (length grid))
         (cols (length (aref grid 0)))
         (gr (mod row rows))
         (gc (mod col cols)))
    ;; Handle negative modulo (Common Lisp mod already returns positive)
    (char (aref grid gr) gc)))

(defun count-reachable (grid start-row start-col steps)
  "Count cells reachable in exactly STEPS steps using BFS."
  (let* ((rows (length grid))
         (cols (length (aref grid 0)))
         (visited (make-hash-table :test 'equal))
         (queue (list (list start-row start-col 0)))
         (target-parity (mod steps 2)))
    ;; Mark start as visited
    (setf (gethash (cons start-row start-col) visited) 0)
    ;; BFS
    (loop while queue
          do (let* ((current (pop queue))
                    (r (first current))
                    (c (second current))
                    (dist (third current)))
               (when (< dist steps)
                 (dolist (delta '((-1 0) (1 0) (0 -1) (0 1)))
                   (let* ((nr (+ r (first delta)))
                          (nc (+ c (second delta)))
                          (key (cons nr nc)))
                     (when (and (>= nr 0) (< nr rows)
                                (>= nc 0) (< nc cols)
                                (not (char= (grid-ref grid nr nc) #\#))
                                (not (gethash key visited)))
                       (setf (gethash key visited) (1+ dist))
                       (setf queue (nconc queue (list (list nr nc (1+ dist)))))))))))
    ;; Count cells with matching parity
    (let ((count 0))
      (maphash (lambda (key d)
                 (declare (ignore key))
                 (when (and (<= d steps) (= (mod d 2) target-parity))
                   (incf count)))
               visited)
      count)))

(defun count-reachable-infinite-bfs (grid start-row start-col steps)
  "Count cells reachable in exactly STEPS steps on infinite tiled grid."
  (let* ((rows (length grid))
         (cols (length (aref grid 0)))
         (visited (make-hash-table :test 'equal))
         (queue (list (list start-row start-col 0)))
         (target-parity (mod steps 2)))
    ;; Mark start as visited
    (setf (gethash (cons start-row start-col) visited) 0)
    ;; BFS
    (loop while queue
          do (let* ((current (pop queue))
                    (r (first current))
                    (c (second current))
                    (dist (third current)))
               (when (< dist steps)
                 (dolist (delta '((-1 0) (1 0) (0 -1) (0 1)))
                   (let* ((nr (+ r (first delta)))
                          (nc (+ c (second delta)))
                          (key (cons nr nc))
                          (gr (mod nr rows))
                          (gc (mod nc cols)))
                     (when (and (not (char= (char (aref grid gr) gc) #\#))
                                (not (gethash key visited)))
                       (setf (gethash key visited) (1+ dist))
                       (setf queue (nconc queue (list (list nr nc (1+ dist)))))))))))
    ;; Count cells with matching parity
    (let ((count 0))
      (maphash (lambda (key d)
                 (declare (ignore key))
                 (when (and (<= d steps) (= (mod d 2) target-parity))
                   (incf count)))
               visited)
      count)))

(defun count-reachable-infinite (grid start-row start-col steps)
  "Count cells reachable in STEPS steps on infinite grid using quadratic pattern."
  (let* ((size (length grid))
         (half (floor size 2)))
    (if (<= steps (* size 2))
        ;; For small step counts, use direct BFS
        (count-reachable-infinite-bfs grid start-row start-col steps)
        ;; For large step counts, use quadratic extrapolation
        (let* ((n (floor (- steps half) size))
               ;; Calculate y0, y1, y2 for quadratic fit
               (y0 (count-reachable-infinite-bfs grid start-row start-col half))
               (y1 (count-reachable-infinite-bfs grid start-row start-col (+ half size)))
               (y2 (count-reachable-infinite-bfs grid start-row start-col (+ half (* 2 size))))
               ;; Solve quadratic: f(x) = ax^2 + bx + c
               ;; Using finite differences:
               ;; y0 = c
               ;; y1 = a + b + c
               ;; y2 = 4a + 2b + c
               ;; => y1 - y0 = a + b
               ;; => y2 - y1 = 3a + b
               ;; => (y2 - y1) - (y1 - y0) = 2a
               ;; => a = (y2 - 2*y1 + y0) / 2
               ;; => b = (y1 - y0) - a
               (a (floor (+ y2 (- (* 2 y1)) y0) 2))
               (b (- y1 y0 a))
               (c y0))
          ;; Return a*n^2 + b*n + c
          (+ (* a n n) (* b n) c)))))

(defun part1 (grid start-row start-col)
  "Part 1: Count plots reachable in exactly 64 steps."
  (count-reachable grid start-row start-col 64))

(defun part2 (grid start-row start-col)
  "Part 2: Count plots reachable in exactly 26501365 steps on infinite grid."
  (count-reachable-infinite grid start-row start-col 26501365))

(defun main ()
  (let ((input-file (merge-pathnames "../input.txt" *load-pathname*)))
    (multiple-value-bind (grid start-row start-col) (read-grid input-file)
      (format t "Part 1: ~a~%" (part1 grid start-row start-col))
      (format t "Part 2: ~a~%" (part2 grid start-row start-col)))))

(main)
