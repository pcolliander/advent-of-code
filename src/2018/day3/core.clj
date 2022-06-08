(ns advent_2018.three.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example
"#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
")

(defn- parse-claim [claim]
  "a claim has the form: #123 @ 3,2: 5x4"
  (let [[_ _ left-and-top width-and-tall] (s/split claim #" ")
      [inches-from-left inches-from-top] (s/split left-and-top #",") 
      [inches-wide inches-tall] (s/split width-and-tall #"x")]

  {:left (parse-long inches-from-left)
   :top  (parse-long (s/replace inches-from-top  #":" ""))
   :wide (parse-long inches-wide)
   :tall (parse-long inches-tall)}))

(defn- coordinates [{:keys [left top tall wide]}]
  (for [x (range left (+ left wide))
        y (range top (+ top tall))]
    [x y]))

(defn part-one
  ([] (part-one (slurp "./src/2018/day3/input.txt")))
  ([input]
   (let [ranges (->> (s/split-lines input)
                     (map parse-claim)
                     (mapcat coordinates))]

     (->> ranges
         frequencies
         vals
         (filter #(< 1 %))
         count))))

(comment
(part-one example)
(part-one))
