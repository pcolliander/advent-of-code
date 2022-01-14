(ns advent-of-code-2021.core
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

(defn- transpose [matrix]
  (apply map vector matrix))

(def matrix '((2 1 9 9 9 4 3 2 1 0)
              (3 9 8 7 8 9 4 9 2 1)
              (9 8 5 6 7 8 9 8 9 2)
              (8 7 6 7 8 9 6 7 8 9)
              (9 8 9 9 9 6 5 6 7 8)))

(def tranposed ([2 3 9 8 9]
                [1 9 8 7 8]
                [9 8 5 6 9]  
                [9 7 6 7 9] ;
                [9 8 7 8 9]
                [4 9 8 9 6]
                [3 4 9 6 5]
                [2 9 8 7 6]
                [1 2 9 8 7]
                [0 1 2 9 8]))

(defn- value-at [x y]
  (nth (nth matrix y) x))

(defn- get-left-right [row pos]
  [(nth row (dec pos) nil) (nth row (inc pos) nil)])

(defn get-adjacent [heightmap columns]
  (->> (for [y (range (count heightmap))
             :let [row (nth heightmap y)]]

         (for [x (range 0 (count row))
               :let [point (nth row x)
                     column (nth columns x)
                     [left right] (get-left-right row x)
                     [up down] (get-left-right column y)]]
           {:point point :left left :down down :right right :up up}))))

(defn- low-point? [{:keys [point up down left right]}]
  (and
    (nil? (some #{point} [left right up down]))
    (= point (apply min (filter some? [point left right up down])))))

(defn low-points [heightmap columns]
  (->> (get-adjacent heightmap columns)
       (mapcat (fn [row]
             (map (fn [adjacent]
                    (when (low-point? adjacent)
                      (:point adjacent)))
                  row)))
       (filter some?))) 

(defn part-one
  ([] (part-one (slurp "./src/2021/day9/input.txt")))
  ([input]
   (let [heightmap (parse input)
         columns (transpose heightmap)]
     (->> (low-points heightmap columns)
          (reduce (fn [acc n]
                    (+ acc (+ 1 n))) 0)))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day9/input.txt")))
  ([input]
   (let [heightmap (parse input)
         columns (transpose heightmap)]

     (->> (for [y (range (count heightmap))
                :let [row (nth heightmap y)]]
            (for [x (range 0 (count row))
                  :let [point (nth row x)
                        column (nth columns x)
                        [left right] (get-left-right row x)
                        [up down] (get-left-right column y)]]
              {:point point :left left :down down :right right :up up}))))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
