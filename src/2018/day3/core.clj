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
  (let [[_ id left top width tall] (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)]
    {:id id
     :left (parse-long left)
     :top  (parse-long top)
     :wide (parse-long width)
     :tall (parse-long tall)}))

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

(defn part-two
  ([] (part-two (slurp "./src/2018/day3/input.txt")))
  ([input]
   (let [claims (map parse-claim (s/split-lines input))
         overlaps (->> claims
                       (mapcat coordinates)
                       frequencies)]

     (some (fn [claim]
             (when (every? (fn [coordinate]
                             (= 1 (get overlaps coordinate))) (coordinates claim))
               (:id claim))) claims))))
(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
