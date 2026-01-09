#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-input [text]
  "Parse valves and tunnels from input text."
  (let [pattern #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)"]
    (reduce
     (fn [{:keys [valves tunnels]} line]
       (let [[_ name rate neighbors] (re-matches pattern line)]
         {:valves (assoc valves name (Integer/parseInt rate))
          :tunnels (assoc tunnels name (str/split neighbors #", "))}))
     {:valves {} :tunnels {}}
     (str/split-lines (str/trim text)))))

(defn bfs-distances [start tunnels relevant-set]
  "Compute shortest distances from start to all relevant valves using BFS."
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited #{start}
         distances {}]
    (if (empty? queue)
      distances
      (let [[curr dist] (peek queue)
            queue (pop queue)
            new-distances (if (and (contains? relevant-set curr) (not= curr start))
                           (assoc distances curr dist)
                           distances)
            neighbors (get tunnels curr [])
            unvisited (filter #(not (visited %)) neighbors)
            new-visited (into visited unvisited)
            new-queue (reduce #(conj %1 [%2 (inc dist)]) queue unvisited)]
        (recur new-queue new-visited new-distances)))))

(defn compute-distances [valves tunnels]
  "Compute shortest distances between all relevant valves."
  (let [relevant (conj (set (filter #(pos? (get valves %)) (keys valves))) "AA")
        relevant-set relevant]
    (reduce
     (fn [distances start]
       (assoc distances start (bfs-distances start tunnels relevant-set)))
     {}
     relevant)))

(defn dfs-part1 [pos time-left opened valves distances valuable memo]
  "DFS with memoization to find maximum pressure release."
  (if (<= time-left 0)
    [0 memo]
    (let [memo-key [pos time-left opened]]
      (if (contains? memo memo-key)
        [(get memo memo-key) memo]
        (let [available (filter #(not (contains? opened %)) valuable)
              [best final-memo]
              (reduce
               (fn [[best-pressure current-memo] next-valve]
                 (let [time-cost (+ 1 (get-in distances [pos next-valve]))
                       new-time (- time-left time-cost)]
                   (if (> new-time 0)
                     (let [pressure (* (get valves next-valve) new-time)
                           [sub-result updated-memo] (dfs-part1 next-valve new-time (conj opened next-valve)
                                                                valves distances valuable current-memo)
                           total (+ pressure sub-result)]
                       [(max best-pressure total) updated-memo])
                     [best-pressure current-memo])))
               [0 memo]
               available)]
          [best (assoc final-memo memo-key best)])))))

(defn part1 [text]
  "Find maximum pressure release in 30 minutes."
  (let [{:keys [valves tunnels]} (parse-input text)
        distances (compute-distances valves tunnels)
        valuable (set (filter #(pos? (get valves %)) (keys valves)))
        [result _] (dfs-part1 "AA" 30 #{} valves distances valuable {})]
    result))

(defn dfs-subset [pos time-left opened subset valves distances memo]
  "DFS for a specific subset of valves."
  (if (<= time-left 0)
    [0 memo]
    (let [memo-key [pos time-left opened]]
      (if (contains? memo memo-key)
        [(get memo memo-key) memo]
        (let [available (filter #(not (contains? opened %)) subset)
              [best final-memo]
              (reduce
               (fn [[best-pressure current-memo] next-valve]
                 (let [dist-val (get-in distances [pos next-valve])
                       time-cost (+ 1 (or dist-val 9999))
                       new-time (- time-left time-cost)]
                   (if (> new-time 0)
                     (let [pressure (* (get valves next-valve) new-time)
                           [sub-result updated-memo] (dfs-subset next-valve new-time (conj opened next-valve)
                                                                  subset valves distances current-memo)
                           total (+ pressure sub-result)]
                       [(max best-pressure total) updated-memo])
                     [best-pressure current-memo])))
               [0 memo]
               available)]
          [best (assoc final-memo memo-key best)])))))

(defn max-pressure-for-subset [subset valves distances]
  "Calculate max pressure for a specific subset of valves."
  (let [[result _] (dfs-subset "AA" 26 #{} (set subset) valves distances {})]
    result))

(defn generate-subsets [coll]
  "Generate all subsets of a collection."
  (reduce
   (fn [subsets elem]
     (into subsets (map #(conj % elem) subsets)))
   #{#{}}
   coll))

(defn part2 [text]
  "Find maximum pressure with elephant helper (26 minutes each)."
  (let [{:keys [valves tunnels]} (parse-input text)
        distances (compute-distances valves tunnels)
        valuable (vec (filter #(pos? (get valves %)) (keys valves)))
        n (count valuable)
        ;; Generate all bitmask subsets
        all-masks (range (bit-shift-left 1 n))
        ;; Compute max pressure for each subset
        mask-to-subset (fn [mask]
                         (set (for [i (range n)
                                    :when (pos? (bit-and mask (bit-shift-left 1 i)))]
                                (get valuable i))))
        max-scores (reduce
                    (fn [scores mask]
                      (assoc scores mask (max-pressure-for-subset (mask-to-subset mask) valves distances)))
                    {}
                    all-masks)
        full-mask (dec (bit-shift-left 1 n))]
    ;; Find best partition
    (reduce
     (fn [best mask]
       (let [complement (bit-xor full-mask mask)]
         (if (<= mask complement)
           (max best (+ (get max-scores mask) (get max-scores complement)))
           best)))
     0
     all-masks)))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
