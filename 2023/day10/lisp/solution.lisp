;;;; Advent of Code 2023 - Day 10: Pipe Maze
;;;; Common Lisp Solution

(defparameter *pipe-connections*
  '((#\| . ((-1 0) (1 0)))    ; N, S
    (#\- . ((0 -1) (0 1)))    ; W, E
    (#\L . ((-1 0) (0 1)))    ; N, E
    (#\J . ((-1 0) (0 -1)))   ; N, W
    (#\7 . ((1 0) (0 -1)))    ; S, W
    (#\F . ((1 0) (0 1)))))   ; S, E

(defun read-input (filename)
  "Read the input file and return a 2D character array."
  (let ((lines (with-open-file (stream filename :direction :input)
                 (loop for line = (read-line stream nil nil)
                       while line
                       collect line))))
    (let* ((rows (length lines))
           (cols (length (first lines)))
           (grid (make-array (list rows cols) :element-type 'character)))
      (loop for line in lines
            for r from 0
            do (loop for c from 0 below (length line)
                     do (setf (aref grid r c) (char line c))))
      grid)))

(defun find-start (grid)
  "Find the starting position 'S' in the grid."
  (destructuring-bind (rows cols) (array-dimensions grid)
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (char= (aref grid r c) #\S)
                     do (return-from find-start (list r c))))))

(defun grid-char (grid r c)
  "Get character at position (r, c) in grid, or nil if out of bounds."
  (destructuring-bind (rows cols) (array-dimensions grid)
    (if (and (>= r 0) (< r rows)
             (>= c 0) (< c cols))
        (aref grid r c)
        nil)))

(defun pipe-connections (ch)
  "Get the connection directions for a pipe character."
  (cdr (assoc ch *pipe-connections*)))

(defun connects-back-p (grid nr nc r c)
  "Check if the pipe at (nr, nc) connects back to (r, c)."
  (let ((adj-ch (grid-char grid nr nc)))
    (when (and adj-ch (pipe-connections adj-ch))
      (loop for (adj-dr adj-dc) in (pipe-connections adj-ch)
            thereis (and (= (+ nr adj-dr) r) (= (+ nc adj-dc) c))))))

(defun get-neighbors (grid pos)
  "Get valid pipe neighbors that connect to this position."
  (destructuring-bind (r c) pos
    (let ((ch (grid-char grid r c)))
      (destructuring-bind (rows cols) (array-dimensions grid)
        (cond
          ((char= ch #\S)
           ;; S can connect to any adjacent pipe that connects back to it
           (loop for (dr dc) in '((-1 0) (1 0) (0 -1) (0 1))
                 for nr = (+ r dr)
                 for nc = (+ c dc)
                 when (and (>= nr 0) (< nr rows)
                           (>= nc 0) (< nc cols)
                           (connects-back-p grid nr nc r c))
                   collect (list nr nc)))
          ((pipe-connections ch)
           (loop for (dr dc) in (pipe-connections ch)
                 for nr = (+ r dr)
                 for nc = (+ c dc)
                 when (and (>= nr 0) (< nr rows)
                           (>= nc 0) (< nc cols))
                   collect (list nr nc)))
          (t nil))))))

;;; Two-list queue for O(1) amortized enqueue/dequeue
(defstruct (queue (:constructor make-queue ()))
  "Efficient FIFO queue using two lists."
  (head nil :type list)
  (tail nil :type list))

(defun queue-empty-p (q)
  "Check if queue is empty."
  (and (null (queue-head q)) (null (queue-tail q))))

(defun queue-enqueue (q item)
  "Add item to the back of the queue."
  (push item (queue-tail q)))

(defun queue-dequeue (q)
  "Remove and return item from the front of the queue."
  (when (null (queue-head q))
    (setf (queue-head q) (nreverse (queue-tail q))
          (queue-tail q) nil))
  (pop (queue-head q)))

(defun find-loop (grid start)
  "BFS to find the main loop. Returns hash table mapping positions to distances."
  (let ((distances (make-hash-table :test 'equal))
        (q (make-queue)))
    (setf (gethash start distances) 0)
    (queue-enqueue q start)
    (loop until (queue-empty-p q)
          for pos = (queue-dequeue q)
          do (loop for neighbor in (get-neighbors grid pos)
                   unless (gethash neighbor distances)
                     do (setf (gethash neighbor distances)
                              (1+ (gethash pos distances)))
                        (queue-enqueue q neighbor)))
    distances))

(defun determine-start-pipe (grid start loop-cells)
  "Determine what pipe type S actually is based on its connections."
  (destructuring-bind (r c) start
    (let ((connections nil))
      (loop for (dr dc) in '((-1 0) (1 0) (0 -1) (0 1))
            for nr = (+ r dr)
            for nc = (+ c dc)
            when (and (gethash (list nr nc) loop-cells)
                      (connects-back-p grid nr nc r c))
              do (push (list dr dc) connections))
      ;; Find matching pipe type
      (loop for (pipe . dirs) in *pipe-connections*
            when (and (= (length dirs) (length connections))
                      (null (set-difference dirs connections :test 'equal))
                      (null (set-difference connections dirs :test 'equal)))
              do (return-from determine-start-pipe pipe))
      #\S)))

(defun north-connection-p (ch)
  "Check if pipe has a north connection (for ray casting)."
  (member ch '(#\| #\L #\J)))

(defun part1 (distances)
  "Find the maximum distance in the loop from precomputed distances."
  (loop for dist being the hash-values of distances
        maximize dist))

(defun part2 (grid start distances)
  "Count tiles enclosed by the loop using ray casting.
   DISTANCES serves dual purpose: maps positions to distances, and acts as set of loop cells."
  (let* ((loop-cells distances)
         (start-pipe (determine-start-pipe grid start loop-cells))
         (enclosed 0))
    (destructuring-bind (rows cols) (array-dimensions grid)
      ;; Create a working copy of the grid with S replaced by its actual pipe type
      (let ((grid-copy (make-array (list rows cols) :element-type 'character)))
        (loop for r from 0 below rows
              do (loop for c from 0 below cols
                       do (setf (aref grid-copy r c) (aref grid r c))))
        (setf (aref grid-copy (first start) (second start)) start-pipe)
        ;; Ray casting: count crossings of loop boundary
        (loop for r from 0 below rows
              do (let ((inside nil))
                   (loop for c from 0 below cols
                         for ch = (aref grid-copy r c)
                         do (if (gethash (list r c) loop-cells)
                                ;; On the loop: toggle inside state when crossing north-connected pipe
                                (when (north-connection-p ch)
                                  (setf inside (not inside)))
                                ;; Not on the loop: count if currently inside
                                (when inside
                                  (incf enclosed))))))))
    enclosed))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-path (merge-pathnames "../input.txt" script-dir))
         (grid (read-input input-path))
         (start (find-start grid))
         (distances (find-loop grid start)))
    (format t "Part 1: ~A~%" (part1 distances))
    (format t "Part 2: ~A~%" (part2 grid start distances))))

(main)
