(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.data.priority-map :refer [priority-map]]))

(def example "1163751742
             1381373672
             2136511328
             3694931569
             7463417111
             1319128137
             1359912421
             3125421639
             1293138521
             2311944581")

(defn- value-at [matrix [x y]]
  (nth (nth matrix y) x))

(defn- get-neighbours [height width [x y]]
  (cond-> []
    (pos? x) (conj [(dec x) y]) ; left
    (< x width) (conj [(inc x) y])
    (pos? y) (conj [x (dec y)]) ; up
    (< y height) (conj [x (inc y)])))

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map (fn [line]
              (map #(Integer/parseInt (str %)) line)))))

(defn- bottom-right [cavern]
  (let [bottom (nth cavern (dec (count cavern)))]
    (nth bottom (dec (count (first cavern))))
    [(dec (count (first cavern)))
     (dec (count cavern))]))

(defn- traverse [cavern]
  (let [bottom-right (bottom-right cavern)
        height (dec (count cavern))
        width  (dec (count (first cavern)))]
    (loop [costs (priority-map [0 0] 0)]
      (println :peek (peek costs))
      (if (= bottom-right (first (peek costs)))
        (get costs bottom-right)
        (let [[pos cost] (peek costs)
              neighbours (for [neighbour-pos (get-neighbours height width pos)
                               :when (nil? (get costs neighbour-pos))
                               :let [node-cost (value-at cavern neighbour-pos)]]
                           [neighbour-pos (+ cost node-cost)])]
          (recur (into (pop costs) neighbours)))))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day15/input.txt")))
  ([input]
   (let [cavern (parse input)]
     (traverse cavern))))

(defn- inc-risk [number]
  (if (> 10 (inc number))
    (inc number)
    1))

(defn- scale-row [row]
  (let [new-row (fn [row] (concat row
                                  (->> (take-last 10 row)
                                       (mapv inc-risk))))]
    (nth (iterate new-row row) 4)))

(defn- scale-tile [tile]
  (let [new-tile (fn [tile] (concat
                              tile
                              (->> (take-last 10 tile)
                                   (map (fn [row]
                                          (map inc-risk row))))))]
    (nth (iterate new-tile tile) 4)))

(defn- expand-cavern [cavern]
  (let [tile (map scale-row cavern) ]
    (scale-tile tile)))

(defn part-two
  ([] (part-two (slurp "./src/2021/day15/input.txt")))
  ([input]
   (let [cavern (parse input)
         expanded-cavern (expand-cavern cavern)]
     (traverse expanded-cavern))))

(comment

;; (scale-row [1 1 6 3 7 5 1 7 4 2])
;;
;; (scale-tile (map scale-row (parse example)))
;;
;; (let [cavern (parse example)
;;       tile (map scale-row cavern)]
;;     (scale-tile tile)) 


(parse example)
(part-one example)
(part-one)
(part-two example)
(part-two))

