(ns advent-of-code-2021.refactored
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "2199943210
             3987894921
             9856789892
             8767896789
             9899965678")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map (fn [line]
              (map #(Integer/parseInt (String/valueOf %)) line)))))

(def matrix '((2 1 9 9 9 4 3 2 1 0)
              (3 9 8 7 8 9 4 9 2 1)
              (9 8 5 6 7 8 9 8 9 2)
              (8 7 6 7 8 9 6 7 8 9)
              (9 8 9 9 9 6 5 6 7 8)))

(defn- value-at [heightmap [x y]]
  (nth (nth heightmap y nil) x nil))

(defn- get-neighbours [heightmap [x y]]
  (cond-> []
    (pos? x) (conj [(dec x) y]) ; left
    (< x (dec (count (first heightmap)))) (conj [(inc x) y])
    (pos? y) (conj [x (dec y)]) ; up
    (< y (dec (count heightmap))) (conj [x (inc y)])))

(defn minimum [heightmap]
  (for [y (range (count heightmap))
        x (range 0 (count (first heightmap)))
        :let [point (value-at heightmap [x y])
              neighbours (->> (get-neighbours heightmap [x y])
                              (map (partial value-at heightmap)))]
        :when (every? #(> % point) neighbours)]
    [x y]))

(defn part-one
    ([] (part-one (slurp "./src/2021/day9/input.txt")))
    ([input]
     (let [heightmap (parse input)]
       (->> (minimum heightmap)
         (map (partial value-at heightmap))
         (reduce #(+ %1 %2 1) 0) ))))


; PART 2
(defn basin [heightmap low-point]
  (let [neighbours (->> (get-neighbours heightmap low-point)
                        (remove (fn [neighbour]
                                     (= 9 (value-at heightmap neighbour)))))]
    (loop [to-visit neighbours
           basin #{low-point}]
      (if-let [value (first to-visit)]
        (let [neighbours (get-neighbours heightmap value)
              flow-downward-neighbours (filter
                                         (fn [neighbour]
                                           (let [neighbour-value (value-at heightmap neighbour)]
                                             (and
                                               (> neighbour-value (value-at heightmap value))
                                               (not= 9 neighbour-value)))) neighbours)]

          (recur (->>
                   (concat (next to-visit) flow-downward-neighbours)
                   (remove #{basin}))
                 (conj basin value)))
        (count basin)))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day9/input.txt")))
  ([input]
   (let [heightmap (parse input)]
     (->> (minimum heightmap)
          (map (partial basin heightmap))
          (sort >)
          (take 3)
          (reduce *)))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
 
