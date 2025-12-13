#!/usr/bin/env sbcl --script

(defconstant +width+ 101)
(defconstant +height+ 103)

(defstruct robot px py vx vy)

(defun extract-numbers (line)
  "Extract all integers (including negative) from a line."
  (let ((numbers '())
        (i 0)
        (len (length line)))
    (loop while (< i len)
          do (let ((char (char line i)))
               (cond
                 ;; Start of a number (digit or minus sign)
                 ((or (digit-char-p char)
                      (and (char= char #\-)
                           (< (1+ i) len)
                           (digit-char-p (char line (1+ i)))))
                  (let ((start i))
                    ;; Skip minus if present
                    (when (char= char #\-)
                      (incf i))
                    ;; Collect digits
                    (loop while (and (< i len) (digit-char-p (char line i)))
                          do (incf i))
                    (push (parse-integer (subseq line start i)) numbers)))
                 ;; Not a number, skip
                 (t (incf i)))))
    (nreverse numbers)))

(defun parse-robots (text)
  "Parse robot positions and velocities from input text."
  (let ((robots '()))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil)
            while line
            do (let ((numbers (extract-numbers line)))
                 (when (= (length numbers) 4)
                   (let ((px (nth 0 numbers))
                         (py (nth 1 numbers))
                         (vx (nth 2 numbers))
                         (vy (nth 3 numbers)))
                     (push (make-robot :px px :py py :vx vx :vy vy) robots))))))
    (nreverse robots)))

(defun simulate (robots seconds)
  "Simulate robot movement for given seconds, return list of positions."
  (loop for robot in robots
        collect (let* ((new-x (mod (+ (robot-px robot) (* (robot-vx robot) seconds)) +width+))
                       (new-y (mod (+ (robot-py robot) (* (robot-vy robot) seconds)) +height+)))
                  (cons new-x new-y))))

(defun count-quadrants (positions)
  "Count robots in each quadrant, excluding middle row/column."
  (let ((mid-x (floor +width+ 2))    ; 50
        (mid-y (floor +height+ 2))   ; 51
        (q1 0) (q2 0) (q3 0) (q4 0))
    (dolist (pos positions)
      (let ((x (car pos))
            (y (cdr pos)))
        (when (and (/= x mid-x) (/= y mid-y))
          (cond ((and (< x mid-x) (< y mid-y)) (incf q1))  ; Top-left
                ((and (> x mid-x) (< y mid-y)) (incf q2))  ; Top-right
                ((and (< x mid-x) (> y mid-y)) (incf q3))  ; Bottom-left
                ((and (> x mid-x) (> y mid-y)) (incf q4)))))) ; Bottom-right
    (list q1 q2 q3 q4)))

(defun part1 (robots)
  "Part 1: Safety factor after 100 seconds."
  (let* ((positions (simulate robots 100))
         (quadrants (count-quadrants positions)))
    (reduce #'* quadrants)))

(defun has-long-horizontal-line (positions threshold)
  "Check if there's a horizontal line of at least THRESHOLD consecutive robots."
  (let ((pos-set (make-hash-table :test 'equal)))
    ;; Build hash set of positions
    (dolist (pos positions)
      (setf (gethash pos pos-set) t))

    ;; Check each row for consecutive robots
    (loop for y from 0 below +height+
          do (let ((max-consecutive 0)
                   (consecutive 0))
               (loop for x from 0 below +width+
                     do (if (gethash (cons x y) pos-set)
                            (progn
                              (incf consecutive)
                              (setf max-consecutive (max max-consecutive consecutive)))
                            (setf consecutive 0)))
               (when (>= max-consecutive threshold)
                 (return-from has-long-horizontal-line t))))
    nil))

(defun part2 (robots)
  "Part 2: Find when robots form a Christmas tree pattern."
  ;; The Christmas tree appears when robots cluster together
  ;; Look for a frame with a long horizontal line of robots (tree base/border)
  (loop for seconds from 1 to (* +width+ +height+)
        do (let ((positions (simulate robots seconds)))
             (when (has-long-horizontal-line positions 20)
               (return seconds)))))

(defun read-input-file (filename)
  "Read input file and return as string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun main ()
  (let* ((input-text (read-input-file "../input.txt"))
         (robots (parse-robots input-text)))
    (format t "Part 1: ~A~%" (part1 robots))
    (format t "Part 2: ~A~%" (part2 robots))))

(main)
