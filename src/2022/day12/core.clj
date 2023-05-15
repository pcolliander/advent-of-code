(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(add-tap println)

(def example
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
")

(defn- at-most-one-higher? [current step]
  (or (>= (int current) (int step))
      (>= 1 (abs (- (int current) (int step))))))

(defn- parse [input]
  (mapv #(mapv identity %) (string/split-lines input)))

(defn- square [area [x y]]
  (nth (nth area y nil) x nil))

(defn- find-squares [area value]
  (for [x (range 0 (-> area first count))
        y (range 0 (count area))
        :when (= value (square area [x y]))]
    [x y]))

(defn- find-square [area value]
  (first (find-squares area value)))

(defn walk [area visited [x y]]
  (let [current-square (square area [x y])
        right [(inc x) y]
        left [(dec x) y]
        up [x (inc y)]
        down [x (dec y)]
        new-paths (->> [right left up down]
                       (remove visited)
                       (filter (partial square area)))]
    (keep (fn [coords]
            (when (at-most-one-higher? current-square (square area coords))
              coords))
          new-paths)))

(defn- find-path [target area start highscore]
  (loop [steps 0
         visited #{}
         nodes [start]]

    (when (< steps highscore)
      (if (some #(= % target) nodes)
        steps
        (let [[visited' nodes'] (reduce (fn [[visited nodes] node]
                                          (let [nodes' (walk area visited node)]
                                            [(into visited nodes')
                                             (concat nodes nodes')]))
                                        [visited []]
                                        nodes)]
          (recur (inc steps)
                 visited'
                 nodes'))))))

(defn part-one
  ([] (part-one (slurp "./src/2022/day12/input.txt")))
  ([input]
   (let [area (parse input)
         start (find-square area \S)
         end (find-square area \E)
         area' (-> area
                   (assoc-in (reverse start) \a)
                   (assoc-in (reverse end)   \z))]
     (find-path end area' start (Integer/MAX_VALUE)))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day12/input.txt")))
  ([input]
   (let [area (parse input)
         end (find-square area \E)
         area' (-> area
                   (assoc-in (reverse (find-square area \S)) \a)
                   (assoc-in (reverse end)   \z))
         
         starting-nodes (find-squares area' \a)]

     (reduce (fn [highscore n]
               (if-let [score (find-path end area' n highscore)]
                 (min highscore score)
                 highscore))
             (Integer/MAX_VALUE)
             starting-nodes))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
