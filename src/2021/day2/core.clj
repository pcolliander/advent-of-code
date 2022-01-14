(ns advent-of-code-2021.two.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]))

(defn- read-input [input-path]
  (let [input (slurp (or input-path "./two/test-input.txt"))]
    (->> (string/split-lines input)
         (map (fn [line]
                (let [[op value] (string/split line #" ")
                      parsed-value (Integer/parseInt value)]
                  [(keyword op) parsed-value]))))))

(defn part-one [input-path]
  (let [lines (read-input input-path)]
    (loop [lines lines
           [x y] [0 0]]
      (if-let [[op value] (first lines)]
        (recur (next lines)
          (case op
            :forward [x (+ y value)]
            :down    [(+ x value) y]
            :up      [(- x value) y]))
        (* x y)))))

(defn part-two [input-path]
  (let [lines (read-input input-path)]
    (loop [lines lines
           [x y aim] [0 0 0]]
      (if-let [[op value] (first lines)]
        (recur (next lines)
               (case op
                 :forward [(+ x (* aim value)) (+ y value) aim]
                 :down    [x y (+ aim value)]
                 :up      [x y (- aim value)]))
        (* x y)))))

(comment
  (part-one "./two/input.txt")
  (part-two "./two/input.txt"))

