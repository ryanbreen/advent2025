#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 - Day 7: No Space Left On Device
;;; Common Lisp Solution

(defun read-input (filename)
  "Read input file and return list of lines."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun parse-filesystem (lines)
  "Parse terminal output and return hash table of directory sizes."
  (let ((path '())
        (dir-sizes (make-hash-table :test 'equal)))
    (dolist (line lines)
      (cond
        ;; cd command
        ((and (>= (length line) 4)
              (string= "$ cd" (subseq line 0 4)))
         (let ((target (subseq line 5)))
           (cond
             ((string= target "/")
              (setf path '("/")))
             ((string= target "..")
              (pop path))
             (t
              (push target path)))))
        ;; ls command - skip
        ((and (>= (length line) 4)
              (string= "$ ls" (subseq line 0 4)))
         nil)
        ;; dir entry - skip
        ((and (>= (length line) 4)
              (string= "dir " (subseq line 0 4)))
         nil)
        ;; file entry
        (t
         (let* ((space-pos (position #\Space line))
                (size (parse-integer (subseq line 0 space-pos))))
           ;; Add size to current directory and all parent directories
           (let ((current-path (reverse path)))
             (loop for i from 1 to (length current-path) do
               (let ((dir-path (build-path (subseq current-path 0 i))))
                 (incf (gethash dir-path dir-sizes 0) size))))))))
    dir-sizes))

(defun build-path (path-list)
  "Build a path string from a list of directories."
  (if (= (length path-list) 1)
      "/"
      (format nil "~{~A~^/~}" path-list)))

(defun part1 (dir-sizes)
  "Sum of sizes of directories with total size <= 100000."
  (let ((total 0))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (<= value 100000)
                 (incf total value)))
             dir-sizes)
    total))

(defun part2 (dir-sizes)
  "Find smallest directory to delete to free enough space."
  (let* ((total-space 70000000)
         (needed-space 30000000)
         (used-space (gethash "/" dir-sizes))
         (free-space (- total-space used-space))
         (need-to-free (- needed-space free-space))
         (candidates '()))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (>= value need-to-free)
                 (push value candidates)))
             dir-sizes)
    (apply #'min candidates)))

(defun main ()
  (let* ((script-path (or *load-truename* *default-pathname-defaults*))
         (script-dir (make-pathname :directory (pathname-directory script-path)))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (lines (read-input input-file))
         (dir-sizes (parse-filesystem lines)))
    (format t "Part 1: ~A~%" (part1 dir-sizes))
    (format t "Part 2: ~A~%" (part2 dir-sizes))))

(main)
