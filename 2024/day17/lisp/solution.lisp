#!/usr/bin/env sbcl --script
;;;; Day 17: Chronospatial Computer - 3-bit VM emulator

(defun parse-input (filename)
  "Parse registers and program from input file."
  (with-open-file (stream filename :direction :input)
    (let* ((line-a (read-line stream))
           (line-b (read-line stream))
           (line-c (read-line stream))
           (_blank (read-line stream nil))
           (line-prog (read-line stream)))
      (declare (ignore _blank))
      (let ((a (parse-integer (subseq line-a (+ 12 (search "Register A: " line-a)))))
            (b (parse-integer (subseq line-b (+ 12 (search "Register B: " line-b)))))
            (c (parse-integer (subseq line-c (+ 12 (search "Register C: " line-c)))))
            (prog-str (subseq line-prog (+ 9 (search "Program: " line-prog)))))
        (values a b c (parse-program prog-str))))))

(defun parse-program (str)
  "Parse comma-separated program string into a list of integers."
  (let ((result nil)
        (current nil))
    (loop for ch across str
          do (if (char= ch #\,)
                 (when current
                   (push (parse-integer (coerce (nreverse current) 'string)) result)
                   (setf current nil))
                 (push ch current)))
    (when current
      (push (parse-integer (coerce (nreverse current) 'string)) result))
    (nreverse result)))

(defun combo-value (operand a b c)
  "Get the value of a combo operand."
  (case operand
    ((0 1 2 3) operand)
    (4 a)
    (5 b)
    (6 c)
    (t (error "Invalid combo operand: ~A" operand))))

(defun execute-vm (a b c program)
  "Execute the 3-bit computer program and return output as a list."
  (let ((prog-vec (coerce program 'vector))
        (ip 0)
        (output nil))
    (loop while (< ip (length prog-vec))
          do (let* ((opcode (aref prog-vec ip))
                    (operand (aref prog-vec (1+ ip)))
                    (jumped nil))
               (case opcode
                 ;; adv - A = A >> combo
                 (0 (setf a (ash a (- (combo-value operand a b c)))))
                 ;; bxl - B = B XOR literal
                 (1 (setf b (logxor b operand)))
                 ;; bst - B = combo % 8
                 (2 (setf b (logand (combo-value operand a b c) 7)))
                 ;; jnz - jump if A != 0
                 (3 (when (/= a 0)
                      (setf ip operand)
                      (setf jumped t)))
                 ;; bxc - B = B XOR C
                 (4 (setf b (logxor b c)))
                 ;; out - output combo % 8
                 (5 (push (logand (combo-value operand a b c) 7) output))
                 ;; bdv - B = A >> combo
                 (6 (setf b (ash a (- (combo-value operand a b c)))))
                 ;; cdv - C = A >> combo
                 (7 (setf c (ash a (- (combo-value operand a b c))))))
               (unless jumped
                 (incf ip 2))))
    (nreverse output)))

(defun format-output (output)
  "Format output list as comma-separated string."
  (format nil "~{~A~^,~}" output))

(defun lists-equal (list1 list2)
  "Check if two lists are equal."
  (and (= (length list1) (length list2))
       (every #'= list1 list2)))

(defun part1 (a b c program)
  "Run the program and return comma-separated output."
  (format-output (execute-vm a b c program)))

(defun part2 (b c program)
  "Find initial A value that makes program output itself.
   Work backwards from the last digit - build A 3 bits at a time."
  (let ((prog-len (length program)))
    (labels ((search-a (target-idx current-a)
               (if (< target-idx 0)
                   current-a
                   (loop for bits from 0 to 7
                         for candidate-a = (logior (ash current-a 3) bits)
                         ;; Skip if A is 0 at start (would halt immediately)
                         unless (and (= candidate-a 0) (= target-idx (1- prog-len)))
                         do (let* ((output (execute-vm candidate-a b c program))
                                   (expected (nthcdr target-idx program)))
                              (when (lists-equal output expected)
                                (let ((result (search-a (1- target-idx) candidate-a)))
                                  (when result
                                    (return-from search-a result)))))
                         finally (return nil)))))
      (search-a (1- prog-len) 0))))

(defun main ()
  "Main entry point."
  (let ((input-path (merge-pathnames "../input.txt" *load-truename*)))
    (multiple-value-bind (a b c program) (parse-input input-path)
      (format t "Part 1: ~A~%" (part1 a b c program))
      (format t "Part 2: ~A~%" (part2 b c program)))))

(main)
