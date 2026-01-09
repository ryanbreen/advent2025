#!/usr/bin/env sbcl --script
;;;; Advent of Code 2022 - Day 18: Boiling Boulders
;;;; Compute surface area of 3D lava droplet

(defun read-input (filename)
  "Read cube coordinates from file, return a hash-table as a set."
  (let ((cubes (make-hash-table :test 'equal)))
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                      (comma1 (position #\, trimmed))
                      (comma2 (position #\, trimmed :start (1+ comma1)))
                      (x (parse-integer (subseq trimmed 0 comma1)))
                      (y (parse-integer (subseq trimmed (1+ comma1) comma2)))
                      (z (parse-integer (subseq trimmed (1+ comma2)))))
                 (setf (gethash (list x y z) cubes) t))))
    cubes))

(defparameter *directions*
  '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1))
  "Six cardinal directions in 3D.")

(defun part1 (cubes)
  "Count total surface area (all exposed faces)."
  (let ((surface-area 0))
    (maphash (lambda (cube value)
               (declare (ignore value))
               (destructuring-bind (x y z) cube
                 (dolist (dir *directions*)
                   (destructuring-bind (dx dy dz) dir
                     (unless (gethash (list (+ x dx) (+ y dy) (+ z dz)) cubes)
                       (incf surface-area))))))
             cubes)
    surface-area))

(defun bounding-box (cubes)
  "Return bounding box with 1-unit padding: (min-x max-x min-y max-y min-z max-z)."
  (let ((min-x most-positive-fixnum) (max-x most-negative-fixnum)
        (min-y most-positive-fixnum) (max-y most-negative-fixnum)
        (min-z most-positive-fixnum) (max-z most-negative-fixnum))
    (maphash (lambda (cube value)
               (declare (ignore value))
               (destructuring-bind (x y z) cube
                 (setf min-x (min min-x x) max-x (max max-x x)
                       min-y (min min-y y) max-y (max max-y y)
                       min-z (min min-z z) max-z (max max-z z))))
             cubes)
    (list (1- min-x) (1+ max-x)
          (1- min-y) (1+ max-y)
          (1- min-z) (1+ max-z))))

(defun part2 (cubes)
  "Count only exterior surface area (excluding trapped air pockets).
   Uses BFS flood fill from outside the bounding box."
  (destructuring-bind (min-x max-x min-y max-y min-z max-z) (bounding-box cubes)
    (let ((exterior (make-hash-table :test 'equal))
          (queue nil))
      ;; Start BFS from corner of bounding box
      (let ((start (list min-x min-y min-z)))
        (setf (gethash start exterior) t)
        (push start queue))
      ;; BFS to find all exterior air cells
      (loop while queue
            do (let ((current (pop queue)))
                 (destructuring-bind (x y z) current
                   (dolist (dir *directions*)
                     (destructuring-bind (dx dy dz) dir
                       (let ((nx (+ x dx))
                             (ny (+ y dy))
                             (nz (+ z dz)))
                         ;; Check bounds
                         (when (and (<= min-x nx max-x)
                                    (<= min-y ny max-y)
                                    (<= min-z nz max-z))
                           (let ((neighbor (list nx ny nz)))
                             ;; Skip cubes and already visited
                             (unless (or (gethash neighbor cubes)
                                         (gethash neighbor exterior))
                               (setf (gethash neighbor exterior) t)
                               (setf queue (nconc queue (list neighbor))))))))))))
      ;; Count faces touching exterior air
      (let ((surface-area 0))
        (maphash (lambda (cube value)
                   (declare (ignore value))
                   (destructuring-bind (x y z) cube
                     (dolist (dir *directions*)
                       (destructuring-bind (dx dy dz) dir
                         (when (gethash (list (+ x dx) (+ y dy) (+ z dz)) exterior)
                           (incf surface-area))))))
                 cubes)
        surface-area))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (cubes (read-input input-file)))
    (format t "Part 1: ~a~%" (part1 cubes))
    (format t "Part 2: ~a~%" (part2 cubes))))

(main)
