;;;; Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer
;;;; Solution in Common Lisp
;;;;
;;;; This problem involves mapping seed numbers through a series of
;;;; range-based transformations to find location numbers.
;;;; Part 1: Find minimum location for individual seed numbers.
;;;; Part 2: Seeds are ranges; find minimum location across all ranges.

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun read-input (filename)
  "Read and return the entire file contents as a string."
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun split-string (string separator)
  "Split STRING by SEPARATOR, returning a list of substrings."
  (loop with sep-len = (length separator)
        for start = 0 then (+ pos sep-len)
        for pos = (search separator string :start2 start)
        collect (subseq string start (or pos (length string)))
        while pos))

(defun trim-whitespace (string)
  "Remove leading and trailing whitespace from STRING."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-numbers (line)
  "Parse a line of space-separated numbers into a list of integers.
   Uses with-input-from-string for idiomatic parsing."
  (with-input-from-string (s line)
    (loop for num = (read s nil nil)
          while num
          collect num)))

;;; ============================================================
;;; Input Parsing
;;; ============================================================

(defun parse-map-section (section)
  "Parse a single map section into a list of (dst-start src-start length) triples.
   Skips the header line (e.g., 'seed-to-soil map:')."
  (loop for line in (rest (split-string (trim-whitespace section)
                                        (format nil "~%")))
        for nums = (parse-numbers line)
        when (= (length nums) 3)
          collect nums))

(defun parse-input (text)
  "Parse input into seeds and list of maps.
   Returns (values seeds maps) where:
   - seeds: list of seed numbers
   - maps: list of maps, each map being a list of range triples"
  (let* ((sections (split-string (trim-whitespace text)
                                 (format nil "~%~%")))
         (seed-line (first sections))
         ;; Extract numbers after the colon in 'seeds: N1 N2 ...'
         (seeds (parse-numbers (subseq seed-line
                                       (1+ (position #\: seed-line)))))
         ;; Parse each map section
         (maps (mapcar #'parse-map-section (rest sections))))
    (values seeds maps)))

;;; ============================================================
;;; Part 1: Single Seed Values
;;; ============================================================

(defun apply-map (value ranges)
  "Apply a single map to transform VALUE through RANGES.
   Each range is (dst-start src-start length).
   If value falls within a source range, it's mapped to the destination.
   Otherwise, the value passes through unchanged (identity mapping)."
  (dolist (range ranges value)
    (destructuring-bind (dst-start src-start length) range
      (when (<= src-start value (1- (+ src-start length)))
        (return-from apply-map (+ dst-start (- value src-start)))))))

(defun seed-to-location (seed maps)
  "Convert a seed number to a location number by applying all maps in sequence."
  (reduce #'apply-map maps :initial-value seed))

(defun part1 (seeds maps)
  "Find the lowest location number for any individual seed.
   Strategy: Map each seed through all transformations, take minimum."
  (loop for seed in seeds
        minimize (seed-to-location seed maps)))

;;; ============================================================
;;; Part 2: Seed Ranges
;;; ============================================================

(defun apply-map-to-ranges (input-ranges map-ranges)
  "Apply a map to a list of ranges, returning new ranges.

   Algorithm: For each input range, split it according to map ranges.
   - Parts overlapping a map range get transformed (offset applied)
   - Parts not overlapping any map range pass through unchanged

   Each input range is (start end) where end is exclusive.
   Each map range is (dst-start src-start length)."
  (let ((result nil))
    (dolist (input-range input-ranges)
      (destructuring-bind (in-start in-end) input-range
        ;; Track remaining unmapped portions as we process each map range
        (let ((remaining (list (list in-start in-end))))
          ;; Try each map range against remaining unmapped portions
          (dolist (map-range map-ranges)
            (destructuring-bind (dst-start src-start length) map-range
              (let ((src-end (+ src-start length))
                    (new-remaining nil))
                (dolist (rem remaining)
                  (destructuring-bind (r-start r-end) rem
                    ;; Portion before map range (stays unmapped for now)
                    (when (< r-start src-start)
                      (push (list r-start (min r-end src-start)) new-remaining))
                    ;; Overlapping portion (transform and add to result)
                    (let ((overlap-start (max r-start src-start))
                          (overlap-end (min r-end src-end)))
                      (when (< overlap-start overlap-end)
                        (let ((offset (- dst-start src-start)))
                          (push (list (+ overlap-start offset)
                                      (+ overlap-end offset))
                                result))))
                    ;; Portion after map range (stays unmapped for now)
                    (when (> r-end src-end)
                      (push (list (max r-start src-end) r-end) new-remaining))))
                (setf remaining new-remaining))))
          ;; Any remaining unmapped portions pass through unchanged
          (dolist (rem remaining)
            (push rem result)))))
    result))

(defun seeds-to-ranges (seeds)
  "Convert seed pairs to ranges: (start length) -> (start end).
   Seeds list is (s1 len1 s2 len2 ...) representing ranges."
  (loop for (start length) on seeds by #'cddr
        collect (list start (+ start length))))

(defun part2 (seeds maps)
  "Find the lowest location for seed ranges.
   Strategy: Convert seeds to ranges, apply each map to split/transform
   ranges, then find the minimum start value across all final ranges."
  (let ((ranges (seeds-to-ranges seeds)))
    ;; Apply each map in sequence, transforming ranges
    (dolist (map-ranges maps)
      (setf ranges (apply-map-to-ranges ranges map-ranges)))
    ;; Minimum location is the smallest range start
    (loop for (start) in ranges minimize start)))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(defun main ()
  "Read input, solve both parts, and print results."
  (let ((input-path (merge-pathnames "../input.txt" *load-truename*)))
    (multiple-value-bind (seeds maps) (parse-input (read-input input-path))
      (format t "Part 1: ~A~%" (part1 seeds maps))
      (format t "Part 2: ~A~%" (part2 seeds maps)))))

(main)
