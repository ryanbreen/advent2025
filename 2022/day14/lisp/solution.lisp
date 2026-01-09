#!/usr/bin/env sbcl --script

;;; Day 14: Regolith Reservoir - Falling Sand Simulation

(defun parse-coord (s)
  "Parse 'x,y' into a cons (x . y)."
  (let ((comma-pos (position #\, s)))
    (cons (parse-integer (subseq s 0 comma-pos))
          (parse-integer (subseq s (1+ comma-pos))))))

(defun parse-path (line)
  "Parse a path line like '498,4 -> 498,6 -> 496,6' into list of (x . y) coords."
  (let ((parts (split-by-arrow line)))
    (mapcar #'parse-coord parts)))

(defun split-by-arrow (s)
  "Split string by ' -> ' delimiter."
  (let ((result nil)
        (start 0)
        (delim " -> ")
        (delim-len 4))
    (loop
      (let ((pos (search delim s :start2 start)))
        (if pos
            (progn
              (push (string-trim '(#\Space) (subseq s start pos)) result)
              (setf start (+ pos delim-len)))
            (progn
              (push (string-trim '(#\Space) (subseq s start)) result)
              (return (nreverse result))))))))

(defun draw-line (rocks x1 y1 x2 y2)
  "Draw a line from (x1,y1) to (x2,y2) into the rocks hash table."
  (if (= x1 x2)
      ;; Vertical line
      (loop for y from (min y1 y2) to (max y1 y2)
            do (setf (gethash (cons x1 y) rocks) t))
      ;; Horizontal line
      (loop for x from (min x1 x2) to (max x1 x2)
            do (setf (gethash (cons x y1) rocks) t))))

(defun parse-paths (text)
  "Parse all rock paths and return (values rocks max-y)."
  (let ((rocks (make-hash-table :test 'equal))
        (max-y 0))
    (dolist (line (split-lines text))
      (when (> (length line) 0)
        (let ((path (parse-path line)))
          (loop for (p1 p2) on path
                while p2
                do (let ((x1 (car p1)) (y1 (cdr p1))
                         (x2 (car p2)) (y2 (cdr p2)))
                     (draw-line rocks x1 y1 x2 y2)
                     (setf max-y (max max-y y1 y2)))))))
    (values rocks max-y)))

(defun split-lines (s)
  "Split string by newlines."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length s)
          when (char= (char s i) #\Newline)
          do (progn
               (push (subseq s start i) result)
               (setf start (1+ i))))
    (when (< start (length s))
      (push (subseq s start) result))
    (nreverse result)))

(defun simulate-sand (blocked max-y floor-p)
  "Simulate one unit of sand falling.
   Returns (x . y) resting position, or nil if falls into abyss."
  (let ((x 500) (y 0))
    (loop
      ;; Check if fallen into abyss (part 1)
      (when (and (not floor-p) (> y max-y))
        (return nil))
      ;; Check floor (part 2)
      (when (and floor-p (= (1+ y) (+ max-y 2)))
        (return (cons x y)))
      ;; Try to move down
      (cond
        ((not (gethash (cons x (1+ y)) blocked))
         (incf y))
        ;; Try down-left
        ((not (gethash (cons (1- x) (1+ y)) blocked))
         (decf x)
         (incf y))
        ;; Try down-right
        ((not (gethash (cons (1+ x) (1+ y)) blocked))
         (incf x)
         (incf y))
        ;; Sand comes to rest
        (t (return (cons x y)))))))

(defun copy-hash-table (ht)
  "Create a copy of a hash table."
  (let ((new-ht (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (k v) (setf (gethash k new-ht) v)) ht)
    new-ht))

(defun part1 (rocks max-y)
  "Count sand units that come to rest before sand falls into abyss."
  (let ((blocked (copy-hash-table rocks))
        (count 0))
    (loop
      (let ((pos (simulate-sand blocked max-y nil)))
        (if (null pos)
            (return count)
            (progn
              (setf (gethash pos blocked) t)
              (incf count)))))))

(defun part2 (rocks max-y)
  "Count sand units until source (500,0) is blocked."
  (let ((blocked (copy-hash-table rocks))
        (count 0)
        (source (cons 500 0)))
    (loop
      (let ((pos (simulate-sand blocked max-y t)))
        (setf (gethash pos blocked) t)
        (incf count)
        (when (equal pos source)
          (return count))))))

(defun read-file (filename)
  "Read entire file as string."
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (text (read-file input-file)))
    (multiple-value-bind (rocks max-y) (parse-paths text)
      (format t "Part 1: ~a~%" (part1 rocks max-y))
      (format t "Part 2: ~a~%" (part2 rocks max-y)))))

(main)
