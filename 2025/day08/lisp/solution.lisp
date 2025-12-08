#!/usr/bin/env sbcl --script

;;; Day 8: Playground - Common Lisp Solution

(defstruct point
  x y z)

(defun split-string (string separator)
  "Split string by separator character."
  (loop with result = '()
        with start = 0
        for pos = (position separator string :start start)
        do (push (subseq string start pos) result)
           (if pos
               (setf start (1+ pos))
               (return (nreverse result)))))

(defun parse-input (filename)
  "Parse the input file and return a list of points."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-trim '(#\Space #\Tab #\Newline) line)
          when (> (length trimmed) 0)
          collect (let* ((parts (split-string trimmed #\,))
                        (x (parse-integer (first parts)))
                        (y (parse-integer (second parts)))
                        (z (parse-integer (third parts))))
                   (make-point :x x :y y :z z)))))

;;; Union-Find Data Structure
(defstruct union-find
  (parent nil :type simple-vector)
  (rank nil :type simple-vector)
  (size nil :type simple-vector))

(defun make-uf (n)
  "Create a new Union-Find structure with n elements."
  (make-union-find
   :parent (coerce (loop for i from 0 below n collect i) 'simple-vector)
   :rank (make-array n :initial-element 0)
   :size (make-array n :initial-element 1)))

(defun uf-find (uf x)
  "Find the root of element x with path compression."
  (let ((parent (union-find-parent uf)))
    (when (/= (svref parent x) x)
      (setf (svref parent x) (uf-find uf (svref parent x))))
    (svref parent x)))

(defun uf-union (uf x y)
  "Union two elements. Returns T if they were in different sets, NIL otherwise."
  (let ((px (uf-find uf x))
        (py (uf-find uf y)))
    (when (= px py)
      (return-from uf-union nil))  ; Already in same set

    (let ((parent (union-find-parent uf))
          (rank (union-find-rank uf))
          (size (union-find-size uf)))
      ;; Union by rank
      (when (< (svref rank px) (svref rank py))
        (rotatef px py))
      (setf (svref parent py) px)
      (incf (svref size px) (svref size py))
      (when (= (svref rank px) (svref rank py))
        (incf (svref rank px)))
      t)))

(defun uf-get-component-sizes (uf)
  "Get sizes of all connected components."
  (let* ((parent (union-find-parent uf))
         (size (union-find-size uf))
         (n (length parent))
         (sizes '()))
    (loop for i from 0 below n
          when (= (svref parent i) i)  ; Root of a component
          do (push (svref size i) sizes))
    sizes))

(defun euclidean-distance-sq (p1 p2)
  "Squared Euclidean distance between two points."
  (+ (expt (- (point-x p1) (point-x p2)) 2)
     (expt (- (point-y p1) (point-y p2)) 2)
     (expt (- (point-z p1) (point-z p2)) 2)))

(defun generate-all-pairs (points)
  "Generate all pairs of point indices with their squared distances."
  (let ((n (length points))
        (pairs '()))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let ((dist-sq (euclidean-distance-sq (nth i points) (nth j points))))
          (push (list dist-sq i j) pairs))))
    pairs))

(defun part1 (points &optional (num-connections 1000))
  "Connect the num-connections closest pairs and return product of 3 largest component sizes."
  (let* ((n (length points))
         (pairs (sort (generate-all-pairs points) #'< :key #'first))
         (uf (make-uf n))
         (connections 0))

    ;; Connect the closest num-connections pairs
    (dolist (pair pairs)
      (destructuring-bind (dist-sq i j) pair
        (declare (ignore dist-sq))
        (uf-union uf i j)
        (incf connections)
        (when (= connections num-connections)
          (return))))

    ;; Get component sizes and find the 3 largest
    (let ((sizes (sort (uf-get-component-sizes uf) #'>)))
      (* (first sizes) (second sizes) (third sizes)))))

(defun part2 (points)
  "Connect all junction boxes into one circuit. Return product of X coordinates of last connection."
  (let* ((n (length points))
         (pairs (sort (generate-all-pairs points) #'< :key #'first))
         (uf (make-uf n))
         (num-components n))

    ;; Connect until all in one circuit
    (dolist (pair pairs)
      (destructuring-bind (dist-sq i j) pair
        (declare (ignore dist-sq))
        (when (uf-union uf i j)  ; Actually merged two components
          (decf num-components)
          (when (= num-components 1)
            ;; This was the last connection
            (return-from part2
              (* (point-x (nth i points))
                 (point-x (nth j points))))))))
    0))

(defun main ()
  (let* ((args sb-ext:*posix-argv*)
         (input-file (if (>= (length args) 2)
                        (second args)
                        "../input.txt"))
         (points (parse-input input-file)))
    (format t "Part 1: ~A~%" (part1 points))
    (format t "Part 2: ~A~%" (part2 points))))

(main)
