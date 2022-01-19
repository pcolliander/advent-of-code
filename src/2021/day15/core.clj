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
    (< x (dec width)) (conj [(inc x) y])
    (pos? y) (conj [x (dec y)]) ; up
    (< y (dec height)) (conj [x (inc y)])))

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (mapv (fn [line]
              (mapv #(Integer/parseInt (str %)) line)))))


(defn- traverse [cavern]
  (let [height (count cavern)
        width  (count (first cavern))]
    (loop [costs (priority-map [0 0] 0)]
      (let [[pos cost] (peek costs)]
        (if (= pos [(dec width) (dec height)])
          cost
          (let [neighbours (for [neighbour-pos (get-neighbours height width pos)
                                 :when (nil? (find costs neighbour-pos))]
                             [neighbour-pos (+ cost (value-at cavern neighbour-pos))])]
            (recur (into (dissoc costs pos) neighbours))))))))

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
  (let [row-count (count row)
        new-row (fn [row] (into
                            []
                            (concat
                              row
                              (->> (take-last row-count row)
                                   (mapv inc-risk)))))]
    (nth (iterate new-row row) 4)))

(defn- scale-tile [tile]
  (let [tile-count (count tile)
        new-tile (fn [tile] (into []
                              (concat
                                tile
                                (->> (take-last tile-count tile)
                                     (mapv (fn [row]
                                             (mapv inc-risk row)))))))]
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
(parse example)
(time (part-one example))
(time (part-one)) ; 3101.96 msescs.
(time (part-two example))
(time (part-two)))

