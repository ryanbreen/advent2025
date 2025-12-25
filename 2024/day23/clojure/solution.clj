#!/usr/bin/env clojure

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-input
  "Parse network connections into an adjacency map."
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce
      (fn [graph line]
        (let [[a b] (str/split (str/trim line) #"-")]
          (-> graph
              (update a (fnil conj #{}) b)
              (update b (fnil conj #{}) a))))
      {}
      (line-seq rdr))))

(defn find-triangles
  "Find all triangles (sets of 3 interconnected nodes)."
  [graph]
  (let [triangles (atom #{})]
    (doseq [a (keys graph)]
      (doseq [b (get graph a)]
        (when (< (compare a b) 0) ; Only process each edge once
          ;; Find common neighbors
          (let [common (set/intersection (get graph a #{}) (get graph b #{}))]
            (doseq [c common]
              ;; Use sorted vector to avoid duplicate triangles
              (swap! triangles conj (vec (sort [a b c]))))))))
    @triangles))

(defn part1
  "Count triangles containing at least one node starting with 't'."
  [graph]
  (let [triangles (find-triangles graph)]
    (count (filter #(some (fn [node] (str/starts-with? node "t")) %) triangles))))

(defn bron-kerbosch
  "Bron-Kerbosch algorithm to find all maximal cliques."
  [graph r p x cliques]
  (if (and (empty? p) (empty? x))
    (swap! cliques conj r)
    (let [pivot-candidates (set/union p x)
          pivot (when (seq pivot-candidates)
                  (apply max-key #(count (set/intersection (get graph % #{}) p)) pivot-candidates))
          pivot-neighbors (get graph pivot #{})
          candidates (set/difference p pivot-neighbors)]
      (reduce
        (fn [[p-state x-state] v]
          (let [neighbors (get graph v #{})]
            (bron-kerbosch
              graph
              (conj r v)
              (set/intersection p-state neighbors)
              (set/intersection x-state neighbors)
              cliques)
            [(disj p-state v) (conj x-state v)]))
        [p x]
        candidates))))

(defn part2
  "Find the largest clique (fully connected subgraph)."
  [graph]
  (let [cliques (atom [])
        all-nodes (set (keys graph))]
    (bron-kerbosch graph #{} all-nodes #{} cliques)
    ;; Find the largest clique
    (let [largest (apply max-key count @cliques)]
      ;; Return sorted, comma-joined password
      (str/join "," (sort largest)))))

(defn -main []
  (let [graph (parse-input "../input.txt")]
    (println "Part 1:" (part1 graph))
    (println "Part 2:" (part2 graph))))

(-main)
