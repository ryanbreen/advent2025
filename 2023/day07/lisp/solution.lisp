;;;; Advent of Code 2023 - Day 7: Camel Cards
;;;; Common Lisp solution

(defun split-string (string separator)
  "Split a string by a single character separator."
  (let ((result nil)
        (start 0))
    (loop for i from 0 to (length string)
          when (or (= i (length string))
                   (char= (char string i) separator))
          do (when (> i start)
               (push (subseq string start i) result))
             (setf start (1+ i)))
    (nreverse result)))

(defun read-input ()
  "Read and parse input file, returning list of (hand . bid) pairs."
  (with-open-file (stream "../input.txt" :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
          collect (let* ((parts (split-string line #\Space))
                         (hand (first parts))
                         (bid (parse-integer (second parts))))
                    (cons hand bid)))))

(defparameter *card-strength* "23456789TJQKA"
  "Card strength order for Part 1 (higher index = stronger).")

(defparameter *card-strength-joker* "J23456789TQKA"
  "Card strength order for Part 2 (J is weakest).")

(defun card-value (card strength-string)
  "Return the strength value of a card (higher = stronger)."
  (position card strength-string))

(defun count-cards (hand)
  "Return a hash table of card counts."
  (let ((counts (make-hash-table :test 'equal)))
    (loop for c across hand
          do (incf (gethash c counts 0)))
    counts))

(defun get-sorted-counts (counts-table)
  "Get sorted list of counts (descending)."
  (let ((counts nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v counts))
             counts-table)
    (sort counts #'>)))

(defun get-hand-type (hand)
  "Return hand type as integer (higher = stronger).
   6 = Five of a kind
   5 = Four of a kind
   4 = Full house
   3 = Three of a kind
   2 = Two pair
   1 = One pair
   0 = High card"
  (let* ((counts-table (count-cards hand))
         (counts (get-sorted-counts counts-table)))
    (cond
      ((equal counts '(5)) 6)           ; Five of a kind
      ((equal counts '(4 1)) 5)         ; Four of a kind
      ((equal counts '(3 2)) 4)         ; Full house
      ((equal counts '(3 1 1)) 3)       ; Three of a kind
      ((equal counts '(2 2 1)) 2)       ; Two pair
      ((equal counts '(2 1 1 1)) 1)     ; One pair
      (t 0))))                          ; High card

(defun get-hand-type-with-jokers (hand)
  "Return hand type with J as wildcards (higher = stronger)."
  (let ((joker-count (count #\J hand)))
    (cond
      ((= joker-count 0) (get-hand-type hand))
      ((= joker-count 5) 6)             ; Five of a kind (all jokers)
      (t
       ;; Count non-joker cards
       (let* ((non-jokers (remove #\J hand))
              (counts-table (count-cards non-jokers))
              (counts (get-sorted-counts counts-table)))
         ;; Add jokers to the highest count
         (setf (first counts) (+ (first counts) joker-count))
         (cond
           ((equal counts '(5)) 6)        ; Five of a kind
           ((equal counts '(4 1)) 5)      ; Four of a kind
           ((equal counts '(3 2)) 4)      ; Full house
           ((equal counts '(3 1 1)) 3)    ; Three of a kind
           ((equal counts '(2 2 1)) 2)    ; Two pair
           ((equal counts '(2 1 1 1)) 1)  ; One pair
           (t 0)))))))                    ; High card

(defun hand-card-values (hand strength-string)
  "Return list of card values for tiebreaking."
  (loop for c across hand
        collect (card-value c strength-string)))

(defun compare-hands (hand1 hand2 type-fn strength-string)
  "Compare two hands. Returns T if hand1 < hand2."
  (let ((type1 (funcall type-fn hand1))
        (type2 (funcall type-fn hand2)))
    (if (/= type1 type2)
        (< type1 type2)
        ;; Same type, compare card by card
        (let ((values1 (hand-card-values hand1 strength-string))
              (values2 (hand-card-values hand2 strength-string)))
          (loop for v1 in values1
                for v2 in values2
                when (/= v1 v2)
                return (< v1 v2)
                finally (return nil))))))

(defun calculate-winnings (hands type-fn strength-string)
  "Sort hands and calculate total winnings."
  (let ((sorted-hands
          (sort (copy-list hands)
                (lambda (a b)
                  (compare-hands (car a) (car b) type-fn strength-string)))))
    (loop for (hand . bid) in sorted-hands
          for rank from 1
          sum (* rank bid))))

(defun part1 (hands)
  "Solve Part 1: Standard hand ranking."
  (calculate-winnings hands #'get-hand-type *card-strength*))

(defun part2 (hands)
  "Solve Part 2: J is a wildcard for type, but weakest for tiebreakers."
  (calculate-winnings hands #'get-hand-type-with-jokers *card-strength-joker*))

(defun main ()
  (let ((hands (read-input)))
    (format t "Part 1: ~a~%" (part1 hands))
    (format t "Part 2: ~a~%" (part2 hands))))

(main)
