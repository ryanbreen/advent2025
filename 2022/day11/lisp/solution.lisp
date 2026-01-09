#!/usr/bin/env sbcl --script

;;; Day 11: Monkey in the Middle

(defstruct monkey
  (items nil)           ; list of worry levels
  (operator #'+)        ; + or *
  (operand nil)         ; number or :old
  (divisor 1)           ; test divisor
  (if-true 0)           ; target monkey if divisible
  (if-false 0)          ; target monkey if not divisible
  (inspections 0))      ; inspection count

(defun parse-items (line)
  "Parse 'Starting items: 79, 98' into list of integers."
  (let ((colon-pos (position #\: line)))
    (when colon-pos
      (let ((items-str (subseq line (1+ colon-pos))))
        (mapcar #'parse-integer
                (remove-if #'(lambda (s) (string= s ""))
                           (mapcar #'(lambda (s) (string-trim '(#\Space #\,) s))
                                   (loop for start = 0 then (1+ end)
                                         for end = (position #\, items-str :start start)
                                         collect (subseq items-str start (or end (length items-str)))
                                         while end))))))))

(defun parse-operation (line)
  "Parse 'Operation: new = old * 19' into operator and operand."
  (let* ((eq-pos (position #\= line))
         (expr (string-trim '(#\Space) (subseq line (1+ eq-pos)))))
    ;; expr is like "old * 19" or "old + old"
    (let* ((parts (remove-if #'(lambda (s) (string= s ""))
                             (loop for start = 0 then (1+ end)
                                   for end = (position #\Space expr :start start)
                                   collect (subseq expr start (or end (length expr)))
                                   while end)))
           (op-str (second parts))
           (operand-str (third parts)))
      (values (if (string= op-str "+") #'+ #'*)
              (if (string= operand-str "old") :old (parse-integer operand-str))))))

(defun parse-last-number (line)
  "Extract the last number from a line."
  (let ((words (remove-if #'(lambda (s) (string= s ""))
                          (loop for start = 0 then (1+ end)
                                for end = (position #\Space line :start start)
                                collect (subseq line start (or end (length line)))
                                while end))))
    (parse-integer (car (last words)))))

(defun parse-monkeys (text)
  "Parse input text into vector of monkey structs."
  (let* ((blocks (loop for start = 0 then (+ end 2)
                       for end = (search (format nil "~%~%") text :start2 start)
                       collect (subseq text start (or end (length text)))
                       while end))
         (monkeys (make-array (length blocks))))
    (loop for block in blocks
          for i from 0
          do (let ((lines (remove-if #'(lambda (s) (string= s ""))
                                     (loop for start = 0 then (1+ end)
                                           for end = (position #\Newline block :start start)
                                           collect (subseq block start (or end (length block)))
                                           while end))))
               (multiple-value-bind (operator operand) (parse-operation (nth 2 lines))
                 (setf (aref monkeys i)
                       (make-monkey
                        :items (parse-items (nth 1 lines))
                        :operator operator
                        :operand operand
                        :divisor (parse-last-number (nth 3 lines))
                        :if-true (parse-last-number (nth 4 lines))
                        :if-false (parse-last-number (nth 5 lines))
                        :inspections 0)))))
    monkeys))

(defun copy-monkeys (monkeys)
  "Create a deep copy of the monkeys array."
  (let ((new-monkeys (make-array (length monkeys))))
    (loop for i from 0 below (length monkeys)
          do (let ((m (aref monkeys i)))
               (setf (aref new-monkeys i)
                     (make-monkey
                      :items (copy-list (monkey-items m))
                      :operator (monkey-operator m)
                      :operand (monkey-operand m)
                      :divisor (monkey-divisor m)
                      :if-true (monkey-if-true m)
                      :if-false (monkey-if-false m)
                      :inspections 0))))
    new-monkeys))

(defun apply-operation (old operator operand)
  "Apply the monkey's operation to the worry level."
  (let ((val (if (eq operand :old) old operand)))
    (funcall operator old val)))

(defun simulate (monkeys rounds &key (relief-divisor 3) (use-modulo nil))
  "Simulate monkey business for given number of rounds."
  (let ((mod-value (when use-modulo
                     (reduce #'* (loop for i from 0 below (length monkeys)
                                       collect (monkey-divisor (aref monkeys i)))))))
    (dotimes (_ rounds)
      (dotimes (i (length monkeys))
        (let ((monkey (aref monkeys i)))
          (dolist (item (monkey-items monkey))
            (incf (monkey-inspections monkey))
            (let ((new-val (apply-operation item
                                            (monkey-operator monkey)
                                            (monkey-operand monkey))))
              ;; Apply relief
              (when (> relief-divisor 1)
                (setf new-val (floor new-val relief-divisor)))
              ;; Apply modulo to prevent overflow
              (when mod-value
                (setf new-val (mod new-val mod-value)))
              ;; Test and throw
              (let ((target (if (zerop (mod new-val (monkey-divisor monkey)))
                                (monkey-if-true monkey)
                                (monkey-if-false monkey))))
                (push new-val (monkey-items (aref monkeys target))))))
          ;; Clear the items after processing (we pushed to targets, now clear source)
          (setf (monkey-items monkey) nil))))
    ;; Reverse all item lists since we used push
    (dotimes (i (length monkeys))
      (setf (monkey-items (aref monkeys i))
            (nreverse (monkey-items (aref monkeys i)))))
    monkeys))

(defun monkey-business (monkeys)
  "Calculate monkey business: product of top 2 inspection counts."
  (let ((inspections (sort (loop for i from 0 below (length monkeys)
                                 collect (monkey-inspections (aref monkeys i)))
                           #'>)))
    (* (first inspections) (second inspections))))

(defun part1 (monkeys)
  "Run 20 rounds with relief (divide by 3)."
  (let ((m (copy-monkeys monkeys)))
    (simulate m 20 :relief-divisor 3)
    (monkey-business m)))

(defun part2 (monkeys)
  "Run 10000 rounds without relief."
  (let ((m (copy-monkeys monkeys)))
    (simulate m 10000 :relief-divisor 1 :use-modulo t)
    (monkey-business m)))

(defun main ()
  (let* ((script-path (or *load-pathname* *compile-file-pathname*
                          (pathname (nth 0 sb-ext:*posix-argv*))))
         (input-file (merge-pathnames "../input.txt" script-path))
         (text (with-open-file (f input-file)
                 (let ((str (make-string (file-length f))))
                   (read-sequence str f)
                   str)))
         (monkeys (parse-monkeys text)))
    (format t "Part 1: ~A~%" (part1 monkeys))
    (format t "Part 2: ~A~%" (part2 monkeys))))

(main)
