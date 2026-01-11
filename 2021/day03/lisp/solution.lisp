;;;; Day 3: Binary Diagnostic - Common Lisp Solution

(defun read-input ()
  "Read binary number strings from input file"
  (let ((input-path (merge-pathnames "../input.txt" *load-truename*)))
    (with-open-file (stream input-path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            when (> (length line) 0)
              collect (string-trim '(#\Space #\Tab #\Return #\Newline) line)))))

(defun count-ones-at-position (numbers pos)
  "Count how many numbers have a '1' at the given position"
  (loop for n in numbers
        count (char= (char n pos) #\1)))

(defun part1 (numbers)
  "Calculate gamma * epsilon for power consumption"
  (let* ((num-bits (length (first numbers)))
         (total (length numbers))
         (gamma 0))
    (dotimes (pos num-bits)
      (let ((ones (count-ones-at-position numbers pos)))
        (when (>= ones (- total ones))
          (setf gamma (logior gamma (ash 1 (- num-bits 1 pos)))))))
    ;; epsilon is bitwise NOT of gamma within num-bits
    (let ((epsilon (logxor gamma (1- (ash 1 num-bits)))))
      (* gamma epsilon))))

(defun find-rating (numbers use-most-common)
  "Find oxygen or CO2 rating by filtering based on bit criteria"
  (let* ((num-bits (length (first numbers)))
         (candidates (copy-list numbers)))
    (dotimes (pos num-bits)
      (when (> (length candidates) 1)
        (let* ((ones (count-ones-at-position candidates pos))
               (zeros (- (length candidates) ones))
               (target (if use-most-common
                           (if (>= ones zeros) #\1 #\0)
                           (if (<= zeros ones) #\0 #\1))))
          (setf candidates (remove-if-not
                            (lambda (n) (char= (char n pos) target))
                            candidates)))))
    (parse-integer (first candidates) :radix 2)))

(defun part2 (numbers)
  "Calculate oxygen * CO2 for life support rating"
  (let ((oxygen (find-rating numbers t))
        (co2 (find-rating numbers nil)))
    (* oxygen co2)))

(defun main ()
  (let ((numbers (read-input)))
    (format t "Part 1: ~a~%" (part1 numbers))
    (format t "Part 2: ~a~%" (part2 numbers))))

(main)
