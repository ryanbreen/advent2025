#!/usr/bin/env sbcl --script
;;;; Advent of Code 2023 - Day 25: Snowverload
;;;; Find the minimum cut of 3 edges that divides the graph into two components.
;;;;
;;;; Uses edge betweenness centrality: edges that form the cut between two large
;;;; components will have high betweenness (many shortest paths pass through them).

(defun split-string (string &optional (separator #\Space))
  "Split a string by separator character."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) separator)
          do (when (> i start)
               (push (subseq string start i) result))
             (setf start (1+ i))
          finally (when (< start (length string))
                    (push (subseq string start) result)))
    (nreverse result)))

(defun parse-input (filename)
  "Parse the input file into an adjacency list representation."
  (let ((graph (make-hash-table :test 'equal)))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
            while line
            when (> (length line) 0)
            do (let* ((colon-pos (position #\: line))
                      (left (subseq line 0 colon-pos))
                      (right (subseq line (+ colon-pos 2)))
                      (neighbors (split-string right)))
                 (dolist (neighbor neighbors)
                   (pushnew neighbor (gethash left graph nil) :test 'equal)
                   (pushnew left (gethash neighbor graph nil) :test 'equal)))))
    graph))

(defun make-edge (a b)
  "Create a normalized edge (sorted pair)."
  (if (string< a b)
      (cons a b)
      (cons b a)))

(defun bfs-component-size (graph start excluded-edges)
  "BFS to find component size, ignoring excluded edges."
  (let ((visited (make-hash-table :test 'equal))
        (queue (list start)))
    (setf (gethash start visited) t)

    (loop while queue
          for node = (pop queue)
          do (dolist (neighbor (gethash node graph))
               (let ((edge (make-edge node neighbor)))
                 (when (and (not (gethash neighbor visited))
                            (not (gethash edge excluded-edges)))
                   (setf (gethash neighbor visited) t)
                   (setf queue (append queue (list neighbor)))))))

    (hash-table-count visited)))

(defun compute-edge-betweenness (graph &optional (sample-size 100))
  "Compute approximate edge betweenness centrality."
  (let ((edge-count (make-hash-table :test 'equal))
        (all-nodes (loop for key being the hash-keys of graph collect key)))

    ;; Sample nodes for efficiency
    (let ((nodes (if (> (length all-nodes) sample-size)
                     (let ((*random-state* (make-random-state t))
                           (selected (make-hash-table :test 'equal)))
                       (loop while (< (hash-table-count selected) sample-size)
                             do (let ((node (nth (random (length all-nodes)) all-nodes)))
                                  (setf (gethash node selected) t)))
                       (loop for key being the hash-keys of selected collect key))
                     all-nodes)))

      (dolist (source nodes)
        ;; BFS to find shortest paths
        (let ((dist (make-hash-table :test 'equal))
              (pred (make-hash-table :test 'equal))
              (queue (list source)))
          (setf (gethash source dist) 0)

          (loop while queue
                for node = (pop queue)
                do (dolist (neighbor (gethash node graph))
                     (cond
                       ((not (gethash neighbor dist))
                        (setf (gethash neighbor dist) (1+ (gethash node dist)))
                        (push node (gethash neighbor pred nil))
                        (setf queue (append queue (list neighbor))))
                       ((= (gethash neighbor dist) (1+ (gethash node dist)))
                        (push node (gethash neighbor pred nil))))))

          ;; Backtrack to count edge usage
          (let ((num-paths (make-hash-table :test 'equal)))
            (setf (gethash source num-paths) 1.0d0)

            ;; Forward pass: count paths
            (let ((sorted-nodes (sort (loop for key being the hash-keys of dist collect key)
                                     #'< :key (lambda (n) (gethash n dist)))))
              (dolist (node sorted-nodes)
                (dolist (p (gethash node pred nil))
                  (incf (gethash node num-paths 0.0d0) (gethash p num-paths 0.0d0)))))

            ;; Backward pass: accumulate edge betweenness
            (let ((dependency (make-hash-table :test 'equal))
                  (sorted-nodes (sort (loop for key being the hash-keys of dist collect key)
                                      #'> :key (lambda (n) (gethash n dist)))))
              (dolist (node sorted-nodes)
                (dolist (p (gethash node pred nil))
                  (let* ((edge (make-edge p node))
                         (frac (/ (gethash p num-paths 1.0d0) (gethash node num-paths 1.0d0)))
                         (contrib (* frac (+ 1.0d0 (gethash node dependency 0.0d0)))))
                    (incf (gethash edge edge-count 0.0d0) contrib)
                    (incf (gethash p dependency 0.0d0) contrib)))))))))

    edge-count))

(defun find-cut-edges (graph)
  "Find the 3 edges to cut using edge betweenness."
  ;; Compute edge betweenness with sampling for speed
  (let* ((edge-betweenness (compute-edge-betweenness graph 100))
         (sorted-edges (sort (loop for key being the hash-keys of edge-betweenness
                                  collect (cons key (gethash key edge-betweenness)))
                            #'> :key #'cdr))
         (total-nodes (hash-table-count graph))
         (top-edges (mapcar #'car (subseq sorted-edges 0 (min 20 (length sorted-edges))))))

    ;; Try removing top candidate edges
    (block search
      (loop for i from 0 below (length top-edges)
            do (loop for j from (1+ i) below (length top-edges)
                    do (loop for k from (1+ j) below (length top-edges)
                            do (let ((excluded (make-hash-table :test 'equal)))
                                 (setf (gethash (nth i top-edges) excluded) t)
                                 (setf (gethash (nth j top-edges) excluded) t)
                                 (setf (gethash (nth k top-edges) excluded) t)

                                 (let* ((start (loop for key being the hash-keys of graph
                                                    return key))
                                        (size1 (bfs-component-size graph start excluded)))
                                   (when (< size1 total-nodes)
                                     ;; Graph is disconnected!
                                     (let ((size2 (- total-nodes size1)))
                                       (return-from search (* size1 size2))))))))))))

(defun part1 (filename)
  "Solve Part 1: Find the 3-edge cut and return product of component sizes."
  (let ((graph (parse-input filename)))
    (find-cut-edges graph)))

(defun part2 (filename)
  "Part 2: Day 25 Part 2 is traditionally unlocked by having 49 stars."
  (declare (ignore filename))
  "Push the big red button!")

(defun main ()
  (let* ((script-path (or *load-truename* *compile-file-truename*))
         (script-dir (make-pathname :directory (pathname-directory script-path)))
         (input-file (merge-pathnames "../input.txt" script-dir)))
    (format t "Part 1: ~a~%" (part1 input-file))
    (format t "Part 2: ~a~%" (part2 input-file))))

(main)
