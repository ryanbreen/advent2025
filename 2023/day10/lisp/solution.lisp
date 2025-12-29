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
  "Read the input file and return a list of strings (grid rows)."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun find-start (grid)
  "Find the starting position 'S' in the grid."
  (loop for row in grid
        for r from 0
        do (loop for c from 0 below (length row)
                 when (char= (char row c) #\S)
                   do (return-from find-start (list r c)))))

(defun grid-char (grid r c)
  "Get character at position (r, c) in grid."
  (if (and (>= r 0) (< r (length grid))
           (>= c 0) (< c (length (nth r grid))))
      (char (nth r grid) c)
      nil))

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
    (let ((ch (grid-char grid r c))
          (rows (length grid))
          (cols (length (nth 0 grid))))
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
        (t nil)))))

(defun find-loop (grid start)
  "BFS to find the main loop and distances from start. Returns a hash table."
  (let ((distances (make-hash-table :test 'equal))
        (queue (list start)))
    (setf (gethash start distances) 0)
    (loop while queue
          for pos = (pop queue)
          do (loop for neighbor in (get-neighbors grid pos)
                   unless (gethash neighbor distances)
                     do (setf (gethash neighbor distances)
                              (1+ (gethash pos distances)))
                        (setf queue (append queue (list neighbor)))))
    distances))

(defun determine-start-pipe (grid start loop-positions)
  "Determine what pipe type S actually is based on its connections."
  (destructuring-bind (r c) start
    (let ((connections nil))
      (loop for (dr dc) in '((-1 0) (1 0) (0 -1) (0 1))
            for nr = (+ r dr)
            for nc = (+ c dc)
            when (and (gethash (list nr nc) loop-positions)
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

(defun part1 (grid)
  "Find the maximum distance in the loop."
  (let* ((start (find-start grid))
         (distances (find-loop grid start)))
    (loop for dist being the hash-values of distances
          maximize dist)))

(defun part2 (grid)
  "Count tiles enclosed by the loop using ray casting."
  (let* ((start (find-start grid))
         (distances (find-loop grid start))
         (loop-positions distances)  ; Using the hash table as a set
         (start-pipe (determine-start-pipe grid start loop-positions))
         (rows (length grid))
         (enclosed 0))
    ;; Create a mutable copy of the grid with S replaced
    (let ((grid-copy (mapcar #'copy-seq grid)))
      (setf (char (nth (first start) grid-copy) (second start)) start-pipe)
      ;; Ray casting
      (loop for r from 0 below rows
            for row = (nth r grid-copy)
            do (let ((inside nil))
                 (loop for c from 0 below (length row)
                       for ch = (char row c)
                       do (if (gethash (list r c) loop-positions)
                              ;; On the loop: toggle if north connection
                              (when (north-connection-p ch)
                                (setf inside (not inside)))
                              ;; Not on the loop: count if inside
                              (when inside
                                (incf enclosed))))))
      enclosed)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-path (merge-pathnames "../input.txt" script-dir))
         (grid (read-input input-path)))
    (format t "Part 1: ~A~%" (part1 grid))
    (format t "Part 2: ~A~%" (part2 grid))))

(main)
