(ns advent-of-code-2021.two.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]))
(def example "forward 5
             down 5
             forward 8
             up 3
             down 8
             forward 2")

(defn- parse [input]
  (->> (string/split-lines input)
       (map (fn [line]
              (let [[op value] (string/split line #" ")
                    parsed-value (Integer/parseInt value)]
                [(keyword op) parsed-value])))))

(defn part-one
  ([]  (part-one (slurp "./src/2021/day2/input.txt")))
  ([input]
   (let [lines (parse input)]
     (loop [lines lines
            [x y] [0 0]]
       (if-let [[op value] (first lines)]
         (recur (next lines)
                (case op
                  :forward [x (+ y value)]
                  :down    [(+ x value) y]
                  :up      [(- x value) y]))
         (* x y))))))

(defn part-two
  ([]  (part-two (slurp "./src/2021/day2/input.txt")))
  ([input]
   (let [lines (parse input)]
     (loop [lines lines
            [x y aim] [0 0 0]]
       (if-let [[op value] (first lines)]
         (recur (next lines)
                (case op
                  :forward [(+ x (* aim value)) (+ y value) aim]
                  :down    [x y (+ aim value)]
                  :up      [x y (- aim value)]))
         (* x y))))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
