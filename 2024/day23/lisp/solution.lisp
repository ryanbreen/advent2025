#!/usr/bin/env sbcl --script

;;; Day 23: LAN Party
;;; Part 1: Find all triangles with at least one node starting with 't'
;;; Part 2: Find the maximum clique using Bron-Kerbosch algorithm

(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (let ((pos (position delimiter string)))
    (if pos
        (list (subseq string 0 pos)
              (subseq string (1+ pos)))
        (list string))))

(defun parse-input (filename)
  "Parse network connections into an adjacency list using hash tables."
  (let ((graph (make-hash-table :test 'equal)))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((parts (split-string (string-trim '(#\Space #\Newline) line) #\-))
                      (a (first parts))
                      (b (second parts)))
                 (when (and a b)
                   ;; Add bidirectional edges
                   (push b (gethash a graph nil))
                   (push a (gethash b graph nil))))))
    graph))

(defun find-triangles (graph)
  "Find all triangles (sets of 3 interconnected nodes)."
  (let ((triangles (make-hash-table :test 'equal))
        (nodes (loop for k being the hash-keys of graph collect k)))
    (dolist (a nodes)
      (let ((neighbors-a (gethash a graph)))
        (dolist (b neighbors-a)
          (when (string< a b)  ;; Process each edge only once
            (let ((neighbors-b (gethash b graph)))
              ;; Find common neighbors (intersection)
              (dolist (c neighbors-a)
                (when (member c neighbors-b :test 'string=)
                  ;; Create sorted triangle key to avoid duplicates
                  (let ((tri (sort (list a b c) #'string<)))
                    (setf (gethash (format nil "~{~A~^,~}" tri) triangles) tri)))))))))
    (loop for tri being the hash-values of triangles collect tri)))

(defun part1 (graph)
  "Count triangles containing at least one node starting with 't'."
  (let ((triangles (find-triangles graph))
        (count 0))
    (dolist (tri triangles)
      (when (some (lambda (node) (char= (char node 0) #\t)) tri)
        (incf count)))
    count))

(defun list-difference (set1 set2)
  "Compute set difference (elements in set1 but not in set2)."
  (remove-if (lambda (x) (member x set2 :test 'string=)) set1))

(defun list-union (set1 set2)
  "Compute set union."
  (remove-duplicates (append set1 set2) :test 'string=))

(defun list-intersection (set1 set2)
  "Compute set intersection."
  (remove-if-not (lambda (x) (member x set2 :test 'string=)) set1))

(defun bron-kerbosch (graph r p x cliques)
  "Bron-Kerbosch algorithm to find all maximal cliques.
   R: current clique being built
   P: candidate nodes that could extend R
   X: already processed nodes
   cliques: accumulator for found cliques"
  (if (and (null p) (null x))
      ;; Found a maximal clique
      (push r cliques)
      ;; Choose pivot from P ∪ X with most connections in P
      (let* ((union-p-x (list-union p x))
             (pivot (when union-p-x
                      (first (sort (copy-list union-p-x) #'>
                                   :key (lambda (v)
                                          (let ((neighbors (gethash v graph)))
                                            (length (list-intersection neighbors p))))))))
             (pivot-neighbors (if pivot (gethash pivot graph) nil))
             (candidates (list-difference p pivot-neighbors))
             (new-p p)
             (new-x x))
        (dolist (v candidates)
          (let ((v-neighbors (gethash v graph)))
            (setf cliques (bron-kerbosch graph
                                         (cons v r)
                                         (list-intersection new-p v-neighbors)
                                         (list-intersection new-x v-neighbors)
                                         cliques))
            (setf new-p (list-difference new-p (list v)))
            (setf new-x (list-union new-x (list v)))))))
  cliques)

(defun part2 (graph)
  "Find the largest clique (fully connected subgraph)."
  (let* ((all-nodes (loop for k being the hash-keys of graph collect k))
         (cliques (bron-kerbosch graph nil all-nodes nil nil)))
    ;; Find the largest clique
    (let ((largest (first (sort cliques #'> :key #'length))))
      ;; Return sorted, comma-joined password
      (format nil "~{~A~^,~}" (sort largest #'string<)))))

(defun main ()
  (let ((graph (parse-input "../input.txt")))
    (format t "Part 1: ~A~%" (part1 graph))
    (format t "Part 2: ~A~%" (part2 graph))))

(main)
