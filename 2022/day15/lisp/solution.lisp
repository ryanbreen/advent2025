#!/usr/bin/env sbcl --script

(defstruct sensor
  sx sy bx by dist)

(defun manhattan-distance (x1 y1 x2 y2)
  "Calculate Manhattan distance between two points."
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defun parse-integer-at (string start)
  "Parse an integer starting at position start, return (value . end-position)."
  (let ((neg nil)
        (pos start)
        (result 0)
        (found nil))
    ;; Skip non-digit, non-minus characters
    (loop while (and (< pos (length string))
                     (not (digit-char-p (char string pos)))
                     (not (char= (char string pos) #\-)))
          do (incf pos))
    (when (>= pos (length string))
      (return-from parse-integer-at nil))
    ;; Check for minus
    (when (char= (char string pos) #\-)
      (setf neg t)
      (incf pos))
    ;; Parse digits
    (loop while (and (< pos (length string))
                     (digit-char-p (char string pos)))
          do (setf result (+ (* result 10) (digit-char-p (char string pos)))
                   found t)
             (incf pos))
    (when found
      (cons (if neg (- result) result) pos))))

(defun extract-all-integers (string)
  "Extract all integers from a string."
  (let ((numbers nil)
        (pos 0))
    (loop
      (let ((result (parse-integer-at string pos)))
        (unless result (return (nreverse numbers)))
        (push (car result) numbers)
        (setf pos (cdr result))))))

(defun parse-line (line)
  "Parse a sensor line and return a sensor struct."
  (let* ((numbers (extract-all-integers line))
         (sx (first numbers))
         (sy (second numbers))
         (bx (third numbers))
         (by (fourth numbers))
         (dist (manhattan-distance sx sy bx by)))
    (make-sensor :sx sx :sy sy :bx bx :by by :dist dist)))

(defun split-lines (text)
  "Split text into lines."
  (let ((lines nil)
        (start 0)
        (len (length text)))
    (loop for i from 0 below len
          when (char= (char text i) #\Newline)
          do (let ((line (subseq text start i)))
               (unless (string= line "")
                 (push line lines)))
             (setf start (1+ i)))
    ;; Handle last line without newline
    (when (< start len)
      (let ((line (subseq text start)))
        (unless (string= line "")
          (push line lines))))
    (nreverse lines)))

(defun parse-sensors (text)
  "Parse all sensor lines from input text."
  (mapcar #'parse-line (split-lines text)))

(defun get-coverage-at-row (sensors row)
  "Get ranges covered by sensors at a specific row."
  (let ((ranges nil))
    (dolist (s sensors)
      (let* ((row-dist (abs (- (sensor-sy s) row))))
        (when (<= row-dist (sensor-dist s))
          (let ((x-spread (- (sensor-dist s) row-dist)))
            (push (cons (- (sensor-sx s) x-spread)
                        (+ (sensor-sx s) x-spread))
                  ranges)))))
    (merge-ranges ranges)))

(defun merge-ranges (ranges)
  "Merge overlapping ranges."
  (if (null ranges)
      nil
      (let ((sorted (sort (copy-list ranges) #'< :key #'car))
            (merged nil))
        (setf merged (list (first sorted)))
        (dolist (r (rest sorted))
          (let ((last (first merged)))
            (if (<= (car r) (1+ (cdr last)))
                (setf (cdr (first merged)) (max (cdr last) (cdr r)))
                (push r merged))))
        (nreverse merged))))

(defun part1 (sensors)
  "Count positions that cannot contain a beacon at row y=2000000."
  (let* ((target-row 2000000)
         (ranges (get-coverage-at-row sensors target-row))
         (total 0)
         (beacons-on-row (make-hash-table :test 'equal)))
    ;; Count total coverage
    (dolist (r ranges)
      (incf total (1+ (- (cdr r) (car r)))))
    ;; Track beacons on this row
    (dolist (s sensors)
      (when (= (sensor-by s) target-row)
        (setf (gethash (sensor-bx s) beacons-on-row) t)))
    ;; Subtract beacons on row
    (- total (hash-table-count beacons-on-row))))

(defun part2 (sensors)
  "Find the distress beacon's tuning frequency."
  (let ((max-coord 4000000))
    (loop for row from 0 to max-coord do
      (let* ((ranges (get-coverage-at-row sensors row))
             (clipped nil))
        ;; Clip ranges to search area
        (dolist (r ranges)
          (unless (or (< (cdr r) 0) (> (car r) max-coord))
            (push (cons (max 0 (car r)) (min max-coord (cdr r))) clipped)))
        (setf clipped (merge-ranges clipped))
        ;; Check if full row is covered
        (unless (and (= (length clipped) 1)
                     (= (caar clipped) 0)
                     (= (cdar clipped) max-coord))
          ;; Found a gap
          (let ((x (if (> (length clipped) 1)
                       (1+ (cdar clipped))
                       (if (> (caar clipped) 0)
                           0
                           (1+ (cdar clipped))))))
            (return-from part2 (+ (* x 4000000) row))))))
    nil))

(defun main ()
  (let* ((script-path (or *load-truename* *default-pathname-defaults*))
         (script-dir (directory-namestring script-path))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (text (with-open-file (stream input-file :direction :input)
                 (let ((contents (make-string (file-length stream))))
                   (read-sequence contents stream)
                   contents)))
         (sensors (parse-sensors text)))
    (format t "Part 1: ~a~%" (part1 sensors))
    (format t "Part 2: ~a~%" (part2 sensors))))

(main)
