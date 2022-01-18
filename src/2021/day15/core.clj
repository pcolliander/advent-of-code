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

(defn- get-neighbours [cavern [x y]]
  (cond-> []
    (pos? x) (conj [(dec x) y]) ; left
    (< x (dec (count (first cavern)))) (conj [(inc x) y])
    (pos? y) (conj [x (dec y)]) ; up
    (< y (dec (count cavern))) (conj [x (inc y)])))

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

(defn part-one
  ([] (part-one (slurp "./src/2021/day15/input.txt")))
  ([input]
   (let [cavern (parse input)
         bottom-right (bottom-right cavern)]
     (loop [costs (priority-map [0 0] 0)]
       (if (= bottom-right (first (peek costs)))
         (get costs bottom-right)
         (let [[pos cost] (peek costs)
               neighbours (->> (get-neighbours cavern pos)
                               (remove costs)
                               (map (fn [neighbour-pos]
                                      (let [node-cost (value-at cavern neighbour-pos)]
                                        [neighbour-pos (+ cost node-cost)]))))]
           (recur (into (pop costs) neighbours))))))))

(comment
(part-one example)
(part-one))
