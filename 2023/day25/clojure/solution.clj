#!/usr/bin/env clojure
(ns day25.solution
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input
  "Parse the input file into a graph (adjacency list as a map of node -> set of neighbors)."
  [filename]
  (with-open [rdr (io/reader filename)]
    (reduce
      (fn [graph line]
        (let [line (str/trim line)]
          (if (empty? line)
            graph
            (let [[left right] (str/split line #": ")
                  neighbors (str/split right #" ")]
              (reduce
                (fn [g neighbor]
                  (-> g
                      (update left (fnil conj #{}) neighbor)
                      (update neighbor (fnil conj #{}) left)))
                graph
                neighbors)))))
      {}
      (line-seq rdr))))

(defn edge-key
  "Create a normalized edge key (sorted pair)."
  [a b]
  (if (< (compare a b) 0)
    [a b]
    [b a]))

(defn bfs-component-size
  "BFS to find component size, ignoring excluded edges."
  [graph start excluded-edges]
  (loop [visited #{start}
         queue (conj clojure.lang.PersistentQueue/EMPTY start)]
    (if (empty? queue)
      (count visited)
      (let [node (peek queue)
            queue' (pop queue)
            neighbors (get graph node #{})]
        (let [[visited' queue'']
              (reduce
                (fn [[vis q] neighbor]
                  (let [edge (edge-key node neighbor)]
                    (if (or (contains? vis neighbor)
                            (contains? excluded-edges edge))
                      [vis q]
                      [(conj vis neighbor) (conj q neighbor)])))
                [visited queue']
                neighbors)]
          (recur visited' queue''))))))

(defn compute-edge-betweenness
  "Compute approximate edge betweenness centrality using sampled BFS."
  [graph sample-nodes]
  (let [all-nodes (keys graph)
        nodes (if (and sample-nodes (> (count all-nodes) sample-nodes))
                (take sample-nodes (shuffle all-nodes))
                all-nodes)]
    (reduce
      (fn [edge-count source]
        ;; BFS to find shortest paths
        (let [;; Build distance map and predecessors
              [dist pred]
              (loop [dist {source 0}
                     pred {}
                     queue (conj clojure.lang.PersistentQueue/EMPTY source)]
                (if (empty? queue)
                  [dist pred]
                  (let [node (peek queue)
                        queue' (pop queue)
                        node-dist (get dist node)
                        neighbors (get graph node #{})]
                    (let [[dist' pred' queue'']
                          (reduce
                            (fn [[d p q] neighbor]
                              (cond
                                (not (contains? d neighbor))
                                [(assoc d neighbor (inc node-dist))
                                 (assoc p neighbor [node])
                                 (conj q neighbor)]

                                (= (get d neighbor) (inc node-dist))
                                [d (update p neighbor conj node) q]

                                :else
                                [d p q]))
                            [dist pred queue']
                            neighbors)]
                      (recur dist' pred' queue'')))))]

          ;; Count number of shortest paths to each node
          (let [num-paths (reduce
                            (fn [paths node]
                              (assoc paths node
                                (if (= node source)
                                  1.0
                                  (reduce
                                    (fn [sum p]
                                      (+ sum (get paths p 0.0)))
                                    0.0
                                    (get pred node [])))))
                            {}
                            (sort-by #(get dist % 0) (keys dist)))

                ;; Accumulate edge betweenness (reverse distance order)
                [edge-count' _]
                (reduce
                  (fn [[ec dependency] node]
                    (let [predecessors (get pred node [])
                          node-paths (get num-paths node 1.0)
                          node-dep (get dependency node 0.0)]
                      (reduce
                        (fn [[ec' dep'] p]
                          (let [edge (edge-key p node)
                                p-paths (get num-paths p 1.0)
                                frac (/ p-paths node-paths)
                                contrib (* frac (+ 1.0 node-dep))]
                            [(update ec' edge (fnil + 0.0) contrib)
                             (update dep' p (fnil + 0.0) contrib)]))
                        [ec dependency]
                        predecessors)))
                  [edge-count {}]
                  (sort-by #(- (get dist % 0)) (keys dist)))]
            edge-count')))
      {}
      nodes)))

(defn find-cut-edges
  "Find the 3 edges to cut using edge betweenness."
  [graph]
  (let [;; Compute edge betweenness with sampling
        edge-betweenness (compute-edge-betweenness graph 100)

        ;; Sort edges by betweenness (highest first)
        sorted-edges (sort-by second > edge-betweenness)
        top-edges (vec (take 20 (map first sorted-edges)))
        total-nodes (count graph)]

    ;; Try combinations of top candidate edges
    (some
      (fn [[i j k]]
        (let [excluded #{(nth top-edges i)
                        (nth top-edges j)
                        (nth top-edges k)}
              start (first (keys graph))
              size1 (bfs-component-size graph start excluded)]
          (when (< size1 total-nodes)
            ;; Graph is disconnected!
            (let [size2 (- total-nodes size1)]
              (* size1 size2)))))
      (for [i (range (count top-edges))
            j (range (inc i) (count top-edges))
            k (range (inc j) (count top-edges))]
        [i j k]))))

(defn part1
  "Solve Part 1: Find the 3-edge cut and return product of component sizes."
  [filename]
  (let [graph (parse-input filename)]
    (find-cut-edges graph)))

(defn part2
  "Part 2: Day 25 Part 2 requires 49 stars - just push the button!"
  [_filename]
  "Push the big red button!")

(defn -main []
  (let [input-file (str (System/getProperty "user.dir") "/../input.txt")]
    (println "Part 1:" (part1 input-file))
    (println "Part 2:" (part2 input-file))))

(-main)
