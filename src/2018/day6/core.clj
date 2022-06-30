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
  "(x1 - x2) + (y1 - y2)"
  (+
   (abs (- x1 x2) )
   (abs (- y1 y2))))

(def equally-far-coordinate [-1 -1])

(defn- finite? [[x y] areas coordinates max-x max-y]
  "whether there are coordinates in its area than isn't its own"
  (let [remaining-coordinates (disj (set (conj coordinates equally-far-coordinate) ) [x y])
        finite-left? (some? (some remaining-coordinates
                                  (for [x' (range x -1 -1)]
                                    (get areas [x' y]))))

        finite-right? (some? (some remaining-coordinates
                                   (for [x' (range x max-x)]
                                    (get areas [x' y]))))

        finite-up? (some? (some remaining-coordinates
                                (for [y' (range y -1 -1)]
                                  (get areas [x y']))))

        finite-down? (some? (some remaining-coordinates
                                  (for [y' (range y max-y)]
                                    (get areas [x y']))))]

    (and finite-left? finite-right? finite-up? finite-down?)))

(defn- parse [input]
  (let [coordinates (->> (s/split-lines input)
                         (map #(-> % (s/split #", ")
                                   (->>
                                     (mapv parse-long)))))
        max-x  (->> coordinates (map first) (apply max) inc)
        max-y (->> coordinates (map second) (apply max) inc)]
    [coordinates max-x max-y]))

(defn part-one
  ([] (part-one (slurp "./src/2018/day6/input.txt")))
  ([input]
   (let [[coordinates max-x max-y] (parse input)
         areas (for [x (range 0 max-x)
                     y (range 0 max-y)
                     :let [top-2 (->> (for [coordinate coordinates]
                                        [coordinate (manhattan-distance coordinate [x y])])
                                      (sort-by second)
                                      (take 2))
                           [closest-coordinate _] (if (apply = (map second top-2))
                                                    [equally-far-coordinate 0]
                                                    (first top-2))]]
                 [[x y] closest-coordinate])]

     (->> areas 
          (keep second)
          frequencies
          (filter #(finite? (first %) (into {} areas) coordinates max-x max-y))
          (apply max-key second)
          second))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day6/input.txt") 10000))
  ([input size]
   (let [[coordinates max-x max-y] (parse input)
         areas (for [x (range 0 max-x)
                     y (range 0 max-y)
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
