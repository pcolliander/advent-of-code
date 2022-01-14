(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.set :as cset]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

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

(defn- neighbours [matrix [x y]]
  (nth (nth matrix y) x))

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map (fn [line]
              (map #(Integer/parseInt (str %)) line)))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day15/input.txt")))
  ([input]
   (let [x (parse input)]
     )))

(comment
(parse example)

(part-one example)
(part-one)
(part-two example)
(part-two))
