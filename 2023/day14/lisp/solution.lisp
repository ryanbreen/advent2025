#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 14: Parabolic Reflector Dish
;;; Tilting platform with rolling rocks (O) and fixed rocks (#)

(defun read-input (filename)
  "Read input file and return a 2D array of characters."
  (with-open-file (stream filename :direction :input)
    (let ((lines (loop for line = (read-line stream nil nil)
                       while line
                       collect line)))
      (let* ((rows (length lines))
             (cols (length (first lines)))
             (grid (make-array (list rows cols) :element-type 'character)))
        (loop for row from 0 below rows
              for line in lines
              do (loop for col from 0 below cols
                       do (setf (aref grid row col) (char line col))))
        grid))))

(defun copy-grid (grid)
  "Create a copy of the grid."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims))
         (new-grid (make-array dims :element-type 'character)))
    (loop for row from 0 below rows
          do (loop for col from 0 below cols
                   do (setf (aref new-grid row col) (aref grid row col))))
    new-grid))

(defun tilt-north (grid)
  "Tilt the grid north, moving all round rocks up."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims)))
    (loop for col from 0 below cols
          do (let ((write-pos 0))
               (loop for row from 0 below rows
                     do (let ((cell (aref grid row col)))
                          (cond
                            ((char= cell #\#)
                             (setf write-pos (1+ row)))
                            ((char= cell #\O)
                             (setf (aref grid row col) #\.)
                             (setf (aref grid write-pos col) #\O)
                             (incf write-pos)))))))))

(defun tilt-south (grid)
  "Tilt the grid south, moving all round rocks down."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims)))
    (loop for col from 0 below cols
          do (let ((write-pos (1- rows)))
               (loop for row from (1- rows) downto 0
                     do (let ((cell (aref grid row col)))
                          (cond
                            ((char= cell #\#)
                             (setf write-pos (1- row)))
                            ((char= cell #\O)
                             (setf (aref grid row col) #\.)
                             (setf (aref grid write-pos col) #\O)
                             (decf write-pos)))))))))

(defun tilt-west (grid)
  "Tilt the grid west, moving all round rocks left."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims)))
    (loop for row from 0 below rows
          do (let ((write-pos 0))
               (loop for col from 0 below cols
                     do (let ((cell (aref grid row col)))
                          (cond
                            ((char= cell #\#)
                             (setf write-pos (1+ col)))
                            ((char= cell #\O)
                             (setf (aref grid row col) #\.)
                             (setf (aref grid row write-pos) #\O)
                             (incf write-pos)))))))))

(defun tilt-east (grid)
  "Tilt the grid east, moving all round rocks right."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims)))
    (loop for row from 0 below rows
          do (let ((write-pos (1- cols)))
               (loop for col from (1- cols) downto 0
                     do (let ((cell (aref grid row col)))
                          (cond
                            ((char= cell #\#)
                             (setf write-pos (1- col)))
                            ((char= cell #\O)
                             (setf (aref grid row col) #\.)
                             (setf (aref grid row write-pos) #\O)
                             (decf write-pos)))))))))

(defun spin-cycle (grid)
  "Perform one spin cycle: N, W, S, E."
  (tilt-north grid)
  (tilt-west grid)
  (tilt-south grid)
  (tilt-east grid))

(defun grid-to-string (grid)
  "Convert grid to a string for hashing."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims))
         (result (make-string (* rows cols))))
    (loop for row from 0 below rows
          do (loop for col from 0 below cols
                   do (setf (char result (+ (* row cols) col))
                            (aref grid row col))))
    result))

(defun calculate-load (grid)
  "Calculate total load on north support beams."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims))
         (total 0))
    (loop for row from 0 below rows
          do (loop for col from 0 below cols
                   do (when (char= (aref grid row col) #\O)
                        (incf total (- rows row)))))
    total))

(defun part1 (grid)
  "Tilt north and calculate load."
  (let ((work-grid (copy-grid grid)))
    (tilt-north work-grid)
    (calculate-load work-grid)))

(defun part2 (grid)
  "Run 1 billion spin cycles and calculate load."
  (let ((work-grid (copy-grid grid))
        (target 1000000000)
        (seen (make-hash-table :test 'equal))
        (cycle-num 0))
    (loop while (< cycle-num target)
          do (let ((state (grid-to-string work-grid)))
               (let ((prev-cycle (gethash state seen)))
                 (when prev-cycle
                   (let* ((cycle-length (- cycle-num prev-cycle))
                          (remaining (mod (- target cycle-num) cycle-length)))
                     (loop repeat remaining
                           do (spin-cycle work-grid))
                     (return-from part2 (calculate-load work-grid))))
                 (setf (gethash state seen) cycle-num)
                 (spin-cycle work-grid)
                 (incf cycle-num))))
    (calculate-load work-grid)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (grid (read-input input-file)))
    (format t "Part 1: ~a~%" (part1 grid))
    (format t "Part 2: ~a~%" (part2 grid))))

(main)
