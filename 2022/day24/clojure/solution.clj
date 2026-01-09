#!/usr/bin/env bb
(ns solution
  (:require [clojure.string :as str]))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn parse-input [text]
  (let [lines (str/split-lines (str/trim text))
        height (count lines)
        width (count (first lines))
        inner-h (- height 2)
        inner-w (- width 2)
        blizzards (for [r (range height)
                       c (range width)
                       :let [ch (get (nth lines r) c)]
                       :when (#{\^ \v \< \>} ch)]
                   [r c ch])
        start [0 (.indexOf (first lines) ".")]
        end [(dec height) (.indexOf (last lines) ".")]]
    {:blizzards blizzards
     :height height
     :width width
     :inner-h inner-h
     :inner-w inner-w
     :start start
     :end end}))

(defn get-blizzard-positions [blizzards inner-h inner-w time]
  (into #{}
    (for [[r c direction] blizzards
          :let [ir (dec r)
                ic (dec c)
                [nr nc] (case direction
                          \^ [(mod (- ir time) inner-h) ic]
                          \v [(mod (+ ir time) inner-h) ic]
                          \< [ir (mod (- ic time) inner-w)]
                          \> [ir (mod (+ ic time) inner-w)])]]
      [(inc nr) (inc nc)])))

(defn precompute-blizzards [blizzards inner-h inner-w period]
  (into {}
    (for [t (range period)]
      [t (get-blizzard-positions blizzards inner-h inner-w t)])))

(defn bfs [{:keys [blizzards height width inner-h inner-w]} start end start-time]
  (let [period (lcm inner-h inner-w)
        blizzard-cache (precompute-blizzards blizzards inner-h inner-w period)
        directions [[0 0] [-1 0] [1 0] [0 -1] [0 1]]]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start-time (first start) (second start)])
           visited #{[(mod start-time period) (first start) (second start)]}]
      (if (empty? queue)
        -1
        (let [[time r c] (peek queue)
              queue (pop queue)]
          (if (= [r c] end)
            time
            (let [next-time (inc time)
                  next-blizzards (get blizzard-cache (mod next-time period))
                  candidates (for [[dr dc] directions
                                   :let [nr (+ r dr)
                                         nc (+ c dc)]
                                   :when (and
                                           ;; Check bounds or start/end
                                           (or (= [nr nc] start)
                                               (= [nr nc] end)
                                               (and (> nr 0)
                                                    (< nr (dec height))
                                                    (> nc 0)
                                                    (< nc (dec width))))
                                           ;; Check not in blizzard
                                           (not (contains? next-blizzards [nr nc])))]
                               [nr nc])
                  new-states (for [[nr nc] candidates
                                   :let [state [(mod next-time period) nr nc]]
                                   :when (not (contains? visited state))]
                               {:state state :entry [next-time nr nc]})]
              (recur
                (reduce (fn [q {:keys [entry]}] (conj q entry)) queue new-states)
                (reduce (fn [v {:keys [state]}] (conj v state)) visited new-states)))))))))

(defn part1 [parsed]
  (bfs parsed (:start parsed) (:end parsed) 0))

(defn part2 [parsed]
  (let [start (:start parsed)
        end (:end parsed)
        t1 (bfs parsed start end 0)
        t2 (bfs parsed end start t1)
        t3 (bfs parsed start end t2)]
    t3))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)
        parsed (parse-input text)]
    (println "Part 1:" (part1 parsed))
    (println "Part 2:" (part2 parsed))))

(-main)
