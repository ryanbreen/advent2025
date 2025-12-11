#!/usr/bin/env -S sbcl --script

(defun parse-input (filename)
  "Parse the input file and return (values rows cols antennas-hash)"
  (with-open-file (stream filename)
    (let* ((grid (loop for line = (read-line stream nil)
                      while line
                      collect line))
           (rows (length grid))
           (cols (if grid (length (first grid)) 0))
           (antennas (make-hash-table :test 'equal)))
      ;; Group antenna positions by frequency
      (loop for r from 0 below rows
            for row in grid
            do (loop for c from 0 below (length row)
                    for ch = (char row c)
                    unless (char= ch #\.)
                      do (push (cons r c) (gethash ch antennas))))
      (values rows cols antennas))))

(defun combinations (list)
  "Generate all pairs of elements from list"
  (loop for (item . rest) on list
        append (loop for other in rest
                    collect (cons item other))))

(defun in-bounds-p (r c rows cols)
  "Check if position is within grid bounds"
  (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

(defun part1 (filename)
  "Calculate antinodes using 2:1 distance ratio.
   Each antenna pair creates two antinodes on opposite sides."
  (multiple-value-bind (rows cols antennas) (parse-input filename)
    (let ((antinodes (make-hash-table :test 'equal)))
      ;; For each frequency
      (maphash (lambda (freq positions)
                 (declare (ignore freq))
                 ;; For each pair of antennas
                 (dolist (pair (combinations positions))
                   (destructuring-bind ((r1 . c1) . (r2 . c2)) pair
                     ;; Antinode formula: extend line from one antenna through the other
                     ;; For 2:1 ratio, antinode is at 2*A - B (double distance from A, opposite direction from B)
                     (let ((ar1 (- (* 2 r1) r2))  ; Antinode beyond antenna 1 (away from antenna 2)
                           (ac1 (- (* 2 c1) c2))
                           (ar2 (- (* 2 r2) r1))  ; Antinode beyond antenna 2 (away from antenna 1)
                           (ac2 (- (* 2 c2) c1)))
                       ;; Add if within bounds
                       (when (in-bounds-p ar1 ac1 rows cols)
                         (setf (gethash (cons ar1 ac1) antinodes) t))
                       (when (in-bounds-p ar2 ac2 rows cols)
                         (setf (gethash (cons ar2 ac2) antinodes) t))))))
               antennas)
      (hash-table-count antinodes))))

(defun part2 (filename)
  "Calculate antinodes at all collinear points along antenna lines.
   Extends in both directions until out of bounds, including antenna positions."
  (multiple-value-bind (rows cols antennas) (parse-input filename)
    (let ((antinodes (make-hash-table :test 'equal)))
      ;; For each frequency
      (maphash (lambda (freq positions)
                 (declare (ignore freq))
                 ;; For each pair of antennas
                 (dolist (pair (combinations positions))
                   (destructuring-bind ((r1 . c1) . (r2 . c2)) pair
                     ;; Calculate the direction vector between antennas
                     (let ((dr (- r2 r1))
                           (dc (- c2 c1)))
                       ;; Direction 1: from antenna 1 towards and beyond antenna 2
                       ;; Step by (dr, dc) starting from antenna 1
                       (loop for r = r1 then (+ r dr)
                             for c = c1 then (+ c dc)
                             while (in-bounds-p r c rows cols)
                             do (setf (gethash (cons r c) antinodes) t))
                       ;; Direction 2: from antenna 1 away from antenna 2
                       ;; Step by (-dr, -dc) starting from antenna 1 - dr
                       (loop for r = (- r1 dr) then (- r dr)
                             for c = (- c1 dc) then (- c dc)
                             while (in-bounds-p r c rows cols)
                             do (setf (gethash (cons r c) antinodes) t))))))
               antennas)
      (hash-table-count antinodes))))

(defun main ()
  (let ((input-file "../input.txt"))
    (format t "Part 1: ~a~%" (part1 input-file))
    (format t "Part 2: ~a~%" (part2 input-file))))

(main)
