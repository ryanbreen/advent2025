#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 Day 16: Proboscidea Volcanium
;;; Valve optimization problem with flow rates and tunnels

(defstruct valve
  name
  flow
  neighbors)

(defun simple-split (string delimiters)
  "Split string on any character in delimiters."
  (let ((result nil)
        (current (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for char across string do
      (if (find char delimiters)
          (progn
            (when (> (length current) 0)
              (push (copy-seq current) result))
            (setf (fill-pointer current) 0))
          (vector-push-extend char current)))
    (when (> (length current) 0)
      (push (copy-seq current) result))
    (nreverse result)))

(defun parse-line (line)
  "Parse a single valve line."
  ;; Line format: Valve XX has flow rate=N; tunnels lead to valves A, B, C
  ;; After splitting on " =;,":
  ;; Valve IK has flow rate 6 tunnels lead to valves EU XY AD SC CH
  ;; 0     1  2   3    4    5 6       7    8  9      10 11 12 13 14
  (let* ((parts (simple-split line " =;,"))
         (name (nth 1 parts))
         (flow (parse-integer (nth 5 parts)))
         (neighbors (nthcdr 10 parts)))
    (make-valve :name name :flow flow :neighbors neighbors)))

(defun parse-input (text)
  "Parse all valves from input text."
  (let ((valves (make-hash-table :test 'equal)))
    (dolist (line (simple-split text '(#\Newline #\Return)))
      (when (> (length line) 0)
        (let ((valve (parse-line line)))
          (setf (gethash (valve-name valve) valves) valve))))
    valves))

(defun compute-distances (valves)
  "Compute shortest distances between all relevant valves using BFS."
  (let* ((relevant (cons "AA"
                         (loop for name being the hash-keys of valves
                               for valve = (gethash name valves)
                               when (> (valve-flow valve) 0)
                               collect name)))
         (distances (make-hash-table :test 'equal)))

    (dolist (start relevant)
      (setf (gethash start distances) (make-hash-table :test 'equal))
      (let ((queue (list (cons start 0)))
            (visited (make-hash-table :test 'equal)))
        (setf (gethash start visited) t)

        (loop while queue do
          (let* ((curr-dist (pop queue))
                 (curr (car curr-dist))
                 (dist (cdr curr-dist))
                 (valve (gethash curr valves)))

            (when (and (member curr relevant :test 'equal)
                       (not (equal curr start)))
              (setf (gethash curr (gethash start distances)) dist))

            (dolist (neighbor (valve-neighbors valve))
              (unless (gethash neighbor visited)
                (setf (gethash neighbor visited) t)
                (setf queue (nconc queue (list (cons neighbor (1+ dist)))))))))))

    distances))

(defun part1 (valves distances)
  "Find maximum pressure release in 30 minutes."
  (let* ((valuable (loop for name being the hash-keys of valves
                         for valve = (gethash name valves)
                         when (> (valve-flow valve) 0)
                         collect name))
         (cache (make-hash-table :test 'equal)))

    (labels ((dfs (pos time-left opened)
               (if (<= time-left 0)
                   0
                   (let ((key (list pos time-left opened)))
                     (or (gethash key cache)
                         (setf (gethash key cache)
                               (let ((best 0))
                                 (dolist (next-valve valuable)
                                   (unless (member next-valve opened :test 'equal)
                                     (let* ((dist-table (gethash pos distances))
                                            (time-cost (+ (gethash next-valve dist-table) 1)))
                                       (when (< time-cost time-left)
                                         (let* ((new-time (- time-left time-cost))
                                                (pressure (* (valve-flow (gethash next-valve valves)) new-time))
                                                (result (+ pressure (dfs next-valve new-time
                                                                         (cons next-valve opened)))))
                                           (setf best (max best result)))))))
                                 best)))))))
      (dfs "AA" 30 nil))))

(defun part2 (valves distances)
  "Find maximum pressure with elephant helper (26 minutes each)."
  (let* ((valuable (coerce
                    (loop for name being the hash-keys of valves
                          for valve = (gethash name valves)
                          when (> (valve-flow valve) 0)
                          collect name)
                    'vector))
         (n (length valuable))
         (max-scores (make-hash-table)))

    ;; Compute max pressure for each subset (represented as bitmask)
    (dotimes (mask (ash 1 n))
      (let ((subset (loop for i below n
                          when (logbitp i mask)
                          collect (aref valuable i)))
            (cache (make-hash-table :test 'equal)))

        (labels ((dfs (pos time-left opened)
                   (if (<= time-left 0)
                       0
                       (let ((key (list pos time-left opened)))
                         (or (gethash key cache)
                             (setf (gethash key cache)
                                   (let ((best 0))
                                     (dolist (next-valve subset)
                                       (unless (member next-valve opened :test 'equal)
                                         (let* ((dist-table (gethash pos distances))
                                                (time-cost (+ (gethash next-valve dist-table) 1)))
                                           (when (< time-cost time-left)
                                             (let* ((new-time (- time-left time-cost))
                                                    (pressure (* (valve-flow (gethash next-valve valves)) new-time))
                                                    (result (+ pressure (dfs next-valve new-time
                                                                             (cons next-valve opened)))))
                                               (setf best (max best result)))))))
                                     best)))))))
          (setf (gethash mask max-scores) (dfs "AA" 26 nil)))))

    ;; Find best partition
    (let ((best 0)
          (full-mask (1- (ash 1 n))))
      (dotimes (mask (ash 1 n))
        (let ((complement (logxor full-mask mask)))
          (when (<= mask complement)
            (setf best (max best (+ (gethash mask max-scores)
                                    (gethash complement max-scores)))))))
      best)))

(defun main ()
  (let* ((script-path (or *load-truename* *compile-file-pathname*
                          (parse-namestring "./solution.lisp")))
         (dir (make-pathname :directory (pathname-directory script-path)))
         (input-path (merge-pathnames "../input.txt" dir))
         (text (with-open-file (f input-path :direction :input)
                 (let ((contents (make-string (file-length f))))
                   (read-sequence contents f)
                   contents)))
         (valves (parse-input text))
         (distances (compute-distances valves)))

    (format t "Part 1: ~A~%" (part1 valves distances))
    (format t "Part 2: ~A~%" (part2 valves distances))))

(main)
