#!/usr/bin/env -S sbcl --script
;;;; Day 21: Keypad Conundrum - Robot chain control with shortest path optimization

;;; Keypad layouts - positions as (row . col)
(defparameter *numeric*
  '((#\7 . (0 . 0)) (#\8 . (0 . 1)) (#\9 . (0 . 2))
    (#\4 . (1 . 0)) (#\5 . (1 . 1)) (#\6 . (1 . 2))
    (#\1 . (2 . 0)) (#\2 . (2 . 1)) (#\3 . (2 . 2))
    (#\0 . (3 . 1)) (#\A . (3 . 2))))

(defparameter *numeric-gap* '(3 . 0))

(defparameter *directional*
  '((#\^ . (0 . 1)) (#\A . (0 . 2))
    (#\< . (1 . 0)) (#\v . (1 . 1)) (#\> . (1 . 2))))

(defparameter *directional-gap* '(0 . 0))

(defun get-position (keypad key)
  "Get position of key in keypad."
  (cdr (assoc key keypad)))

(defun equal-pos (pos1 pos2)
  "Check if two positions are equal."
  (and (= (car pos1) (car pos2))
       (= (cdr pos1) (cdr pos2))))

(defun shortest-paths (keypad gap start end)
  "Find all shortest paths from start to end, avoiding gap."
  (let* ((start-pos (get-position keypad start))
         (end-pos (get-position keypad end))
         (sr (car start-pos))
         (sc (cdr start-pos))
         (er (car end-pos))
         (ec (cdr end-pos))
         (paths nil))

    (labels ((dfs (r c path)
               (when (equal-pos (cons r c) gap)
                 (return-from dfs))
               (when (equal-pos (cons r c) end-pos)
                 (push (coerce (reverse path) 'string) paths)
                 (return-from dfs))
               ;; Move vertically toward target
               (cond ((< r er) (dfs (1+ r) c (cons #\v path)))
                     ((> r er) (dfs (1- r) c (cons #\^ path))))
               ;; Move horizontally toward target
               (cond ((< c ec) (dfs r (1+ c) (cons #\> path)))
                     ((> c ec) (dfs r (1- c) (cons #\< path))))))

      (dfs sr sc nil)
      (if paths paths (list ""))))) ; Empty path if start == end

(defvar *memo* (make-hash-table :test 'equal))

(defun min-presses-for-move (from-char to-char depth is-numeric)
  "Minimum presses needed to move from from-char to to-char and press, at given depth."
  (let ((key (list from-char to-char depth is-numeric)))
    (or (gethash key *memo*)
        (setf (gethash key *memo*)
              (let* ((keypad (if is-numeric *numeric* *directional*))
                     (gap (if is-numeric *numeric-gap* *directional-gap*))
                     (paths (shortest-paths keypad gap from-char to-char)))

                (if (= depth 0)
                    ;; At human level, just return path length + 1 for 'A' press
                    (1+ (reduce #'min (mapcar #'length paths)))

                    ;; Otherwise recurse through the directional keypad above
                    (let ((best most-positive-fixnum))
                      (dolist (path paths)
                        ;; Need to type path + 'A' on the directional keypad above
                        (let ((sequence (concatenate 'string path "A"))
                              (cost 0)
                              (current #\A))
                          (loop for char across sequence do
                            (incf cost (min-presses-for-move current char (1- depth) nil))
                            (setf current char))
                          (setf best (min best cost))))
                      best)))))))

(defun solve-code (code depth)
  "Compute minimum presses to type code on numeric keypad with given robot depth."
  (let ((total 0)
        (current #\A))
    (loop for char across code do
      (incf total (min-presses-for-move current char depth t))
      (setf current char))
    total))

(defun complexity (code length)
  "Compute complexity: length * numeric part of code."
  (let ((numeric-part (parse-integer code :junk-allowed t)))
    (* length numeric-part)))

(defun part1 (codes)
  "Part 1: 2 intermediate robots (depth = 2)."
  (let ((total 0))
    (dolist (code codes)
      (let ((length (solve-code code 2)))
        (incf total (complexity code length))))
    total))

(defun part2 (codes)
  "Part 2: 25 intermediate robots (depth = 25)."
  (let ((total 0))
    (dolist (code codes)
      (let ((length (solve-code code 25)))
        (incf total (complexity code length))))
    total))

(defun read-input (filename)
  "Read and parse input file."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          when (string/= (string-trim '(#\Space #\Tab #\Newline) line) "")
          collect (string-trim '(#\Space #\Tab #\Newline) line))))

(defun main ()
  (let ((codes (read-input "../input.txt")))
    ;; Clear memo between parts
    (clrhash *memo*)
    (format t "Part 1: ~A~%" (part1 codes))
    (clrhash *memo*)
    (format t "Part 2: ~A~%" (part2 codes))))

(main)
