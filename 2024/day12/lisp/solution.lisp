#!/usr/bin/env sbcl --script

(defun read-input (filename)
  "Read the grid from the input file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect (coerce line 'list))))

(defun get-cell (grid r c)
  "Get the cell at row r, col c in the grid (or NIL if out of bounds)."
  (let ((rows (length grid))
        (cols (length (first grid))))
    (if (and (>= r 0) (< r rows) (>= c 0) (< c cols))
        (nth c (nth r grid))
        nil)))

(defun find-regions (grid)
  "Find all connected regions in the grid using BFS."
  (let ((rows (length grid))
        (cols (length (first grid)))
        (visited (make-hash-table :test 'equal))
        (regions nil))

    (loop for r from 0 below rows do
      (loop for c from 0 below cols do
        (let ((pos (cons r c)))
          (unless (gethash pos visited)
            ;; BFS to find all cells in this region
            (let ((plant (get-cell grid r c))
                  (region (make-hash-table :test 'equal))
                  (queue (list pos)))

              (loop while queue do
                (let* ((current (pop queue))
                       (cr (car current))
                       (cc (cdr current)))

                  (unless (or (gethash current visited)
                             (null (get-cell grid cr cc))
                             (not (char= (get-cell grid cr cc) plant)))

                    (setf (gethash current visited) t)
                    (setf (gethash current region) t)

                    ;; Add neighbors to queue
                    (dolist (delta '((0 . 1) (0 . -1) (1 . 0) (-1 . 0)))
                      (let* ((nr (+ cr (car delta)))
                             (nc (+ cc (cdr delta)))
                             (npos (cons nr nc)))
                        (unless (gethash npos visited)
                          (push npos queue)))))))

              (when (> (hash-table-count region) 0)
                (push region regions)))))))

    regions))

(defun calculate-perimeter (region)
  "Calculate perimeter of a region (edges not touching same region)."
  (let ((perimeter 0))
    (maphash (lambda (pos val)
               (declare (ignore val))
               (let ((r (car pos))
                     (c (cdr pos)))
                 (dolist (delta '((0 . 1) (0 . -1) (1 . 0) (-1 . 0)))
                   (let* ((nr (+ r (car delta)))
                          (nc (+ c (cdr delta)))
                          (npos (cons nr nc)))
                     (unless (gethash npos region)
                       (incf perimeter))))))
             region)
    perimeter))

(defun count-sides (region)
  "Count number of sides (corners) in a region."
  (let ((corners 0))
    (maphash (lambda (pos val)
               (declare (ignore val))
               (let* ((r (car pos))
                      (c (cdr pos))
                      ;; Check all 8 neighbors
                      (up (gethash (cons (- r 1) c) region))
                      (down (gethash (cons (+ r 1) c) region))
                      (left (gethash (cons r (- c 1)) region))
                      (right (gethash (cons r (+ c 1)) region))
                      (up-left (gethash (cons (- r 1) (- c 1)) region))
                      (up-right (gethash (cons (- r 1) (+ c 1)) region))
                      (down-left (gethash (cons (+ r 1) (- c 1)) region))
                      (down-right (gethash (cons (+ r 1) (+ c 1)) region)))

                 ;; Top-left corner
                 (when (or (and (not up) (not left))  ; convex
                          (and up left (not up-left)))  ; concave
                   (incf corners))

                 ;; Top-right corner
                 (when (or (and (not up) (not right))  ; convex
                          (and up right (not up-right)))  ; concave
                   (incf corners))

                 ;; Bottom-left corner
                 (when (or (and (not down) (not left))  ; convex
                          (and down left (not down-left)))  ; concave
                   (incf corners))

                 ;; Bottom-right corner
                 (when (or (and (not down) (not right))  ; convex
                          (and down right (not down-right)))  ; concave
                   (incf corners))))
             region)
    corners))

(defun part1 (grid)
  "Calculate total fencing cost: sum of area * perimeter for each region."
  (let ((regions (find-regions grid))
        (total 0))
    (dolist (region regions)
      (let ((area (hash-table-count region))
            (perimeter (calculate-perimeter region)))
        (incf total (* area perimeter))))
    total))

(defun part2 (grid)
  "Calculate total fencing cost using sides instead of perimeter."
  (let ((regions (find-regions grid))
        (total 0))
    (dolist (region regions)
      (let ((area (hash-table-count region))
            (sides (count-sides region)))
        (incf total (* area sides))))
    total))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt"
                                      (make-pathname :name nil :type nil
                                                    :defaults *load-truename*)))
         (grid (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 grid))
    (format t "Part 2: ~A~%" (part2 grid))))

(main)
