#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-filesystem
  "Parse terminal output and return map of directory path to total size."
  [lines]
  (loop [lines lines
         path []
         dir-sizes {}]
    (if (empty? lines)
      dir-sizes
      (let [line (first lines)
            rest-lines (rest lines)]
        (cond
          ;; cd command
          (str/starts-with? line "$ cd")
          (let [target (subs line 5)]
            (cond
              (= target "/")
              (recur rest-lines ["/"] dir-sizes)

              (= target "..")
              (recur rest-lines (vec (butlast path)) dir-sizes)

              :else
              (recur rest-lines (conj path target) dir-sizes)))

          ;; ls command - skip
          (str/starts-with? line "$ ls")
          (recur rest-lines path dir-sizes)

          ;; dir entry - skip
          (str/starts-with? line "dir ")
          (recur rest-lines path dir-sizes)

          ;; file entry - add size to all parent directories
          :else
          (let [size (Long/parseLong (first (str/split line #" ")))
                ;; Build all path prefixes and add size to each
                updated-sizes (reduce
                               (fn [sizes i]
                                 (let [dir-path (str/join "/" (take (inc i) path))]
                                   (update sizes dir-path (fnil + 0) size)))
                               dir-sizes
                               (range (count path)))]
            (recur rest-lines path updated-sizes)))))))

(defn part1
  "Sum of sizes of directories with total size <= 100000."
  [dir-sizes]
  (->> (vals dir-sizes)
       (filter #(<= % 100000))
       (reduce +)))

(defn part2
  "Find smallest directory to delete to free enough space."
  [dir-sizes]
  (let [total-space 70000000
        needed-space 30000000
        used-space (get dir-sizes "/")
        free-space (- total-space used-space)
        need-to-free (- needed-space free-space)]
    (->> (vals dir-sizes)
         (filter #(>= % need-to-free))
         (apply min))))

(defn -main []
  (let [script-dir (.getParent (java.io.File. *file*))
        input-file (str script-dir "/../input.txt")
        lines (-> (slurp input-file)
                  str/trim
                  (str/split #"\n"))
        dir-sizes (parse-filesystem lines)]
    (println "Part 1:" (part1 dir-sizes))
    (println "Part 2:" (part2 dir-sizes))))

(-main)
