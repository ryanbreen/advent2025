#!/usr/bin/env sbcl --script

(defun gcd* (a b)
  "Compute greatest common divisor."
  (if (zerop b)
      a
      (gcd* b (mod a b))))

(defun lcm* (a b)
  "Compute least common multiple."
  (/ (* a b) (gcd* a b)))

(defun split-string (string separator)
  "Split a string by a separator character."
  (loop for start = 0 then (1+ end)
        for end = (position separator string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun read-file (filename)
  "Read entire file contents as a string."
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun parse-input (text)
  "Parse the map and extract blizzard positions.
   Returns (blizzards height width inner-h inner-w start end)."
  (let* ((lines (remove-if (lambda (s) (or (string= s "") (string= s (string #\Return))))
                           (split-string text #\Newline)))
         (height (length lines))
         (width (length (first lines)))
         (inner-h (- height 2))
         (inner-w (- width 2))
         (blizzards '())
         (start nil)
         (end nil))
    ;; Find blizzards
    (loop for r from 0 below height
          for line in lines
          do (loop for c from 0 below (length line)
                   for ch = (char line c)
                   do (when (member ch '(#\^ #\v #\< #\>))
                        (push (list r c ch) blizzards))))
    ;; Find start (dot in first row)
    (setf start (cons 0 (position #\. (first lines))))
    ;; Find end (dot in last row)
    (setf end (cons (1- height) (position #\. (car (last lines)))))
    (values (nreverse blizzards) height width inner-h inner-w start end)))

(defun get-blizzard-positions (blizzards inner-h inner-w time)
  "Get all blizzard positions at a given time as a hash set."
  (let ((positions (make-hash-table :test 'equal)))
    (dolist (bliz blizzards)
      (destructuring-bind (r c direction) bliz
        (let* ((ir (1- r))  ; inner row
               (ic (1- c))  ; inner col
               (nr ir)
               (nc ic))
          (cond
            ((char= direction #\^)
             (setf nr (mod (- ir time) inner-h)))
            ((char= direction #\v)
             (setf nr (mod (+ ir time) inner-h)))
            ((char= direction #\<)
             (setf nc (mod (- ic time) inner-w)))
            ((char= direction #\>)
             (setf nc (mod (+ ic time) inner-w))))
          ;; Convert back to full coordinates
          (setf (gethash (cons (1+ nr) (1+ nc)) positions) t))))
    positions))

(defun precompute-blizzards (blizzards inner-h inner-w period)
  "Precompute blizzard positions for all times in one period."
  (let ((cache (make-array period)))
    (dotimes (t* period)
      (setf (aref cache t*) (get-blizzard-positions blizzards inner-h inner-w t*)))
    cache))

(defun bfs (blizzards height width inner-h inner-w start end start-time)
  "BFS to find shortest path avoiding blizzards."
  (let* ((period (lcm* inner-h inner-w))
         (blizzard-cache (precompute-blizzards blizzards inner-h inner-w period))
         (queue (list (list start-time (car start) (cdr start))))
         (visited (make-hash-table :test 'equal))
         (directions '((0 . 0) (-1 . 0) (1 . 0) (0 . -1) (0 . 1))))  ; wait, up, down, left, right
    ;; Mark start as visited
    (setf (gethash (list (mod start-time period) (car start) (cdr start)) visited) t)

    (loop while queue do
      (let* ((current (pop queue))
             (time (first current))
             (r (second current))
             (c (third current)))
        ;; Check if we reached the end
        (when (and (= r (car end)) (= c (cdr end)))
          (return-from bfs time))

        (let* ((next-time (1+ time))
               (next-blizzards (aref blizzard-cache (mod next-time period))))
          (dolist (dir directions)
            (let ((nr (+ r (car dir)))
                  (nc (+ c (cdr dir))))
              ;; Check bounds
              (when (or (and (= nr (car start)) (= nc (cdr start)))  ; start position
                        (and (= nr (car end)) (= nc (cdr end)))      ; end position
                        (and (> nr 0) (< nr (1- height))             ; inner area
                             (> nc 0) (< nc (1- width))))
                ;; Check blizzards
                (unless (gethash (cons nr nc) next-blizzards)
                  (let ((state (list (mod next-time period) nr nc)))
                    (unless (gethash state visited)
                      (setf (gethash state visited) t)
                      (setf queue (nconc queue (list (list next-time nr nc)))))))))))))
    -1))  ; No path found

(defun part1 (text)
  "Find shortest path from start to end."
  (multiple-value-bind (blizzards height width inner-h inner-w start end)
      (parse-input text)
    (bfs blizzards height width inner-h inner-w start end 0)))

(defun part2 (text)
  "Find shortest path: start -> end -> start -> end."
  (multiple-value-bind (blizzards height width inner-h inner-w start end)
      (parse-input text)
    ;; Trip 1: start to end
    (let* ((t1 (bfs blizzards height width inner-h inner-w start end 0))
           ;; Trip 2: end to start
           (t2 (bfs blizzards height width inner-h inner-w end start t1))
           ;; Trip 3: start to end again
           (t3 (bfs blizzards height width inner-h inner-w start end t2)))
      t3)))

(defun main ()
  (let* ((args sb-ext:*posix-argv*)
         (script-path (or (second args) "solution.lisp"))
         (script-dir (directory-namestring (truename script-path)))
         (input-file (concatenate 'string script-dir "../input.txt"))
         (text (read-file input-file)))
    (format t "Part 1: ~a~%" (part1 text))
    (format t "Part 2: ~a~%" (part2 text))))

(main)
