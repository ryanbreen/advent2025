#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 15: Lens Library
;;; HASH algorithm and HASHMAP procedure for lens configuration

(defun hash-algorithm (s)
  "Run the HASH algorithm on a string.
   For each character: current = ((current + ASCII) * 17) % 256"
  (let ((current 0))
    (loop for c across s
          do (setf current (mod (* (+ current (char-code c)) 17) 256)))
    current))

(defun parse-input (filename)
  "Read input file and return a list of comma-separated steps."
  (with-open-file (stream filename :direction :input)
    (let ((text (make-string-output-stream)))
      (loop for line = (read-line stream nil nil)
            while line
            do (write-string line text))
      (let ((input-str (get-output-stream-string text)))
        ;; Split by commas
        (loop with result = nil
              with start = 0
              for i from 0 to (length input-str)
              when (or (= i (length input-str))
                       (char= (char input-str i) #\,))
              do (when (< start i)
                   (push (subseq input-str start i) result))
                 (setf start (1+ i))
              finally (return (nreverse result)))))))

(defun part1 (steps)
  "Sum of HASH values for all steps."
  (loop for step in steps
        sum (hash-algorithm step)))

(defun find-lens-position (box label)
  "Find the position of a lens with given label in the box, or nil if not found."
  (position label box :key #'car :test #'string=))

(defun part2 (steps)
  "Run HASHMAP procedure and calculate focusing power.
   Each box contains a list of (label . focal-length) pairs."
  (let ((boxes (make-array 256 :initial-element nil)))
    ;; Process each step
    (loop for step in steps
          do (let ((equals-pos (position #\= step))
                   (dash-pos (position #\- step)))
               (cond
                 ;; '=' operation: add or replace lens
                 (equals-pos
                  (let* ((label (subseq step 0 equals-pos))
                         (focal (parse-integer (subseq step (1+ equals-pos))))
                         (box-num (hash-algorithm label))
                         (box (aref boxes box-num))
                         (existing-pos (find-lens-position box label)))
                    (if existing-pos
                        ;; Replace existing lens
                        (setf (cdr (nth existing-pos box)) focal)
                        ;; Add new lens at end
                        (setf (aref boxes box-num)
                              (append box (list (cons label focal)))))))
                 ;; '-' operation: remove lens
                 (dash-pos
                  (let* ((label (subseq step 0 dash-pos))
                         (box-num (hash-algorithm label)))
                    (setf (aref boxes box-num)
                          (remove label (aref boxes box-num)
                                  :key #'car :test #'string=)))))))

    ;; Calculate focusing power
    (loop for box-num from 0 below 256
          for box = (aref boxes box-num)
          sum (loop for lens in box
                    for slot from 1
                    sum (* (1+ box-num) slot (cdr lens))))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (steps (parse-input input-file)))
    (format t "Part 1: ~a~%" (part1 steps))
    (format t "Part 2: ~a~%" (part2 steps))))

(main)
