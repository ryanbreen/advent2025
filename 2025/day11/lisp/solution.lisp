#!/usr/bin/env sbcl --script

;;; Advent of Code 2025 Day 11: Reactor
;;; Count paths through a directed acyclic graph

(require :uiop)

(defun parse-input (filename)
  "Parse input file into a hash table graph (adjacency list)."
  (let ((graph (make-hash-table :test 'equal)))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) line)))
                 (when (> (length trimmed) 0)
                   (let* ((colon-pos (position #\: trimmed))
                          (node (subseq trimmed 0 colon-pos))
                          (rest (string-trim '(#\Space) (subseq trimmed (1+ colon-pos))))
                          (neighbors (if (> (length rest) 0)
                                        (uiop:split-string rest :separator " ")
                                        '())))
                     (setf (gethash node graph) neighbors))))))
    graph))

(defun part1 (graph)
  "Count all paths from 'you' to 'out' using memoization."
  (let ((memo (make-hash-table :test 'equal)))
    (labels ((count-paths (node)
               (cond
                 ;; Check memo first
                 ((gethash node memo)
                  (gethash node memo))
                 ;; Base case: reached 'out'
                 ((string= node "out")
                  (setf (gethash node memo) 1)
                  1)
                 ;; Node not in graph
                 ((not (gethash node graph))
                  (setf (gethash node memo) 0)
                  0)
                 ;; Recursive case: sum paths through neighbors
                 (t
                  (let ((total (loop for neighbor in (gethash node graph)
                                     sum (count-paths neighbor))))
                    (setf (gethash node memo) total)
                    total)))))
      (count-paths "you"))))

(defun part2 (graph)
  "Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'.
   Uses the formula:
   paths(svr->dac) * paths(dac->fft) * paths(fft->out) +
   paths(svr->fft) * paths(fft->dac) * paths(dac->out)"
  (labels ((make-path-counter (target)
             "Create a memoized path counter to a specific target."
             (let ((memo (make-hash-table :test 'equal)))
               (labels ((count-paths (node)
                          (cond
                            ;; Check memo first
                            ((gethash node memo)
                             (gethash node memo))
                            ;; Base case: reached target
                            ((string= node target)
                             (setf (gethash node memo) 1)
                             1)
                            ;; Node not in graph
                            ((not (gethash node graph))
                             (setf (gethash node memo) 0)
                             0)
                            ;; Recursive case: sum paths through neighbors
                            (t
                             (let ((total (loop for neighbor in (gethash node graph)
                                                sum (count-paths neighbor))))
                               (setf (gethash node memo) total)
                               total)))))
                 #'count-paths))))

    (let ((paths-to-out (make-path-counter "out"))
          (paths-to-dac (make-path-counter "dac"))
          (paths-to-fft (make-path-counter "fft")))

      ;; Paths that visit dac before fft: svr -> dac -> fft -> out
      (let ((dac-before-fft (* (funcall paths-to-dac "svr")
                               (funcall paths-to-fft "dac")
                               (funcall paths-to-out "fft")))
            ;; Paths that visit fft before dac: svr -> fft -> dac -> out
            (fft-before-dac (* (funcall paths-to-fft "svr")
                               (funcall paths-to-dac "fft")
                               (funcall paths-to-out "dac"))))
        (+ dac-before-fft fft-before-dac)))))

(defun main ()
  (let* ((input-file (if (> (length sb-ext:*posix-argv*) 1)
                         (second sb-ext:*posix-argv*)
                         "../input.txt"))
         (graph (parse-input input-file)))

    (format t "Part 1: ~A~%" (part1 graph))
    (format t "Part 2: ~A~%" (part2 graph))))

(main)
