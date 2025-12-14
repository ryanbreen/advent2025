(require '[clojure.string :as str])

;; Read and parse input
(def input-text (slurp "../input.txt"))
(def grid (mapv vec (str/split-lines input-text)))
(def rows (count grid))
(def cols (count (first grid)))

;; Direction vectors for up, down, left, right
(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn in-bounds? [r c]
  "Check if position is within grid bounds."
  (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

(defn get-cell [r c]
  "Get the cell value at position [r c]."
  (when (in-bounds? r c)
    (get-in grid [r c])))

(defn find-regions []
  "Find all connected regions in the grid using BFS."
  (loop [visited #{}
         r 0
         c 0
         regions []]
    (cond
      ;; Done scanning entire grid
      (>= r rows) regions

      ;; Move to next row
      (>= c cols) (recur visited (inc r) 0 regions)

      ;; Already visited this cell
      (visited [r c]) (recur visited r (inc c) regions)

      ;; Start BFS from this cell
      :else
      (let [plant (get-cell r c)
            ;; BFS to find all cells in this region
            [new-visited region]
            (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [r c])
                   visited visited
                   region #{}]
              (if (empty? queue)
                [visited region]
                (let [[cr cc] (peek queue)
                      queue' (pop queue)]
                  (cond
                    ;; Already visited
                    (visited [cr cc])
                    (recur queue' visited region)

                    ;; Out of bounds or wrong plant type
                    (or (not (in-bounds? cr cc))
                        (not= (get-cell cr cc) plant))
                    (recur queue' visited region)

                    ;; Valid cell in region
                    :else
                    (let [visited' (conj visited [cr cc])
                          region' (conj region [cr cc])
                          ;; Add neighbors to queue
                          neighbors (for [[dr dc] directions]
                                      [(+ cr dr) (+ cc dc)])
                          queue'' (reduce conj queue'
                                          (remove visited' neighbors))]
                      (recur queue'' visited' region'))))))]
        (recur new-visited r (inc c) (conj regions region))))))

(defn calculate-perimeter [region]
  "Calculate perimeter of a region (edges not touching same region)."
  (reduce +
    (for [[r c] region
          [dr dc] directions
          :let [nr (+ r dr)
                nc (+ c dc)]
          :when (not (region [nr nc]))]
      1)))

(defn count-sides [region]
  "Count number of sides (corners) in a region.
   A corner is either:
   - Convex: both orthogonal neighbors are outside the region
   - Concave: both orthogonal neighbors are inside, but diagonal is outside"
  (reduce +
    (for [[r c] region]
      (let [up (region [(dec r) c])
            down (region [(inc r) c])
            left (region [r (dec c)])
            right (region [r (inc c)])
            up-left (region [(dec r) (dec c)])
            up-right (region [(dec r) (inc c)])
            down-left (region [(inc r) (dec c)])
            down-right (region [(inc r) (inc c)])

            ;; Count corners for this cell
            corners (cond-> 0
                      ;; Top-left corner
                      (or (and (not up) (not left))           ; convex
                          (and up left (not up-left)))        ; concave
                      inc

                      ;; Top-right corner
                      (or (and (not up) (not right))          ; convex
                          (and up right (not up-right)))      ; concave
                      inc

                      ;; Bottom-left corner
                      (or (and (not down) (not left))         ; convex
                          (and down left (not down-left)))    ; concave
                      inc

                      ;; Bottom-right corner
                      (or (and (not down) (not right))        ; convex
                          (and down right (not down-right)))  ; concave
                      inc)]
        corners))))

(defn part1 []
  "Calculate total fencing cost: sum of area * perimeter for each region."
  (let [regions (find-regions)]
    (reduce +
      (for [region regions]
        (let [area (count region)
              perimeter (calculate-perimeter region)]
          (* area perimeter))))))

(defn part2 []
  "Calculate total fencing cost using sides instead of perimeter."
  (let [regions (find-regions)]
    (reduce +
      (for [region regions]
        (let [area (count region)
              sides (count-sides region)]
          (* area sides))))))

;; Main
(println "Part 1:" (part1))
(println "Part 2:" (part2))
