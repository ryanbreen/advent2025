#!/usr/bin/env sbcl --script
;;; Day 22: Monkey Market - Pseudorandom number generation for market prices

(defun next-secret (secret)
  "Generate the next secret number using mix and prune operations."
  (let ((s secret))
    ;; Step 1: multiply by 64, mix (XOR), prune (mod 16777216)
    (setf s (logxor s (ash s 6)))           ; s ^= (s << 6)
    (setf s (logand s #xFFFFFF))            ; s &= 0xFFFFFF (mod 16777216)

    ;; Step 2: divide by 32, mix, prune
    (setf s (logxor s (ash s -5)))          ; s ^= (s >> 5)
    (setf s (logand s #xFFFFFF))

    ;; Step 3: multiply by 2048, mix, prune
    (setf s (logxor s (ash s 11)))          ; s ^= (s << 11)
    (setf s (logand s #xFFFFFF))

    s))

(defun generate-secrets (initial count)
  "Generate a sequence of secret numbers."
  (let ((secrets (list initial))
        (secret initial))
    (dotimes (i count)
      (setf secret (next-secret secret))
      (push secret secrets))
    (nreverse secrets)))

(defun part1 (initial-secrets)
  "Sum of the 2000th secret number for each buyer."
  (let ((total 0))
    (dolist (initial initial-secrets)
      (let ((secret initial))
        (dotimes (i 2000)
          (setf secret (next-secret secret)))
        (incf total secret)))
    total))

(defun part2 (initial-secrets)
  "Find the best sequence of 4 price changes to maximize bananas."
  ;; Map from (change1 change2 change3 change4) -> total bananas
  (let ((sequence-totals (make-hash-table :test 'equal)))

    (dolist (initial initial-secrets)
      ;; Generate 2001 secrets (initial + 2000 new)
      (let* ((secrets (generate-secrets initial 2000))
             (prices (mapcar (lambda (s) (mod s 10)) secrets))
             (changes nil))

        ;; Calculate changes
        (loop for i from 0 below (1- (length prices))
              do (push (- (nth (1+ i) prices) (nth i prices)) changes))
        (setf changes (nreverse changes))

        ;; Track first occurrence of each 4-change sequence for this buyer
        (let ((seen (make-hash-table :test 'equal)))
          (loop for i from 0 to (- (length changes) 4)
                do (let ((seq (list (nth i changes)
                                   (nth (+ i 1) changes)
                                   (nth (+ i 2) changes)
                                   (nth (+ i 3) changes))))
                     (unless (gethash seq seen)
                       (setf (gethash seq seen) t)
                       ;; Price we get is after these 4 changes
                       (incf (gethash seq sequence-totals 0)
                             (nth (+ i 4) prices))))))))

    ;; Return maximum value
    (let ((max-bananas 0))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (when (> v max-bananas)
                   (setf max-bananas v)))
               sequence-totals)
      max-bananas)))

(defun read-input (filename)
  "Read initial secret numbers from input file."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          when (> (length (string-trim '(#\Space #\Tab #\Newline) line)) 0)
          collect (parse-integer line))))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (initial-secrets (read-input input-file)))
    (format t "Part 1: ~a~%" (part1 initial-secrets))
    (format t "Part 2: ~a~%" (part2 initial-secrets))))

(main)
