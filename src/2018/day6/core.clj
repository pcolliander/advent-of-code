(ns advent_2018.six.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example 
"1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

(defn- manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2) )
     (abs (- y1 y2))))

(defn- parse [input]
  (let [coordinates (->> (s/split-lines input)
                         (map #(-> % (s/split #", ")
                                   (->>
                                     (mapv parse-long)))))
        max-x  (->> coordinates (map first) (apply max))
        max-y (->> coordinates (map second) (apply max))]
    [coordinates max-x max-y]))

(defn- boundaries [area max-x max-y]
  (->> area
       (filter (fn [[[x y] letter]]
                 (or (= x 0) (= x max-x) (= y 0) (= y max-y))))
       (map second)
       (set)))

(defn- coordinate->letter [c]
  (str (char (+ (int \a) c))))

(defn part-one
  ([] (part-one (slurp "./src/2018/day6/input.txt")))
  ([input]
   (let [[coordinates max-x max-y] (parse input)
         area (for [x (range 0 (inc max-x))
                    y (range 0 (inc max-y))
                    :let [top-2 (->> coordinates
                                     (map-indexed (fn [index coordinate]
                                                    [(coordinate->letter index) (manhattan-distance coordinate [x y])]))
                                           (sort-by second)
                                           (take 2))
                          closest-coordinate  (if (apply = (map second top-2))
                                                       "."
                                                       (ffirst top-2))]]
                [[x y] closest-coordinate])
         boundaries (boundaries area max-x max-y)
         freqs (->> area
                    (keep second)
                    frequencies)]

         (apply max-key second (apply dissoc freqs boundaries)))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day6/input.txt") 10000))
  ([input size]
   (let [[coordinates max-x max-y] (parse input)
         areas (for [x (range 0 (inc max-x))
                     y (range 0 (inc max-y))
                     :let [distances-total (reduce + (for [coordinate coordinates]
                                                       (manhattan-distance coordinate [x y])))]
                     :when (< distances-total size)]
                 [x y])]
     (count areas))))

(comment
(part-one example)
(part-one)
(part-two example 32)
(part-two))
