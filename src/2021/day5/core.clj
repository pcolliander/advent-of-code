(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "0,9 -> 5,9
             8,0 -> 0,9
             9,4 -> 3,4
             2,2 -> 2,1
             7,0 -> 7,4
             6,4 -> 2,0
             0,9 -> 2,9
             3,4 -> 1,4
             0,0 -> 8,8
             5,5 -> 8,2")

(defn- parse [input]
  (->> lines
       (string/split-lines input)
       (map #(string/split % #" -> "))
       (map string/trim)
       (map (fn [[f s]]
              (letfn [(parse-coordinates [coordinate-pair]
                        (let [[f s] (string/split coordinate-pair #",")]
                          [(Integer/parseInt f) (Integer/parseInt s)]))]
                [(parse-coordinates f) (parse-coordinates s)])))))

(defn- horizontal-and-vertical? [[[x1 y1] [x2 y2]]]
  (or
    (= x1 x2)
    (= y1 y2)))

(defn- horizontal-and-vertical [coordinates]
  (->> coordinates
       (filter horizontal-and-vertical?)
       (mapcat (fn [[[x1 y1] [x2 y2]]]
                 (cond
                   (and
                     (= x1 x2)
                     (= y1 y2)) [x1 y1]

                   (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))]
                               [x y1])
                   (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))]
                               [x1 y]))))))

(defn dups [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(defn part-one
  ([] (part-one (slurp "./src/2021/day5/input.txt")))
  ([input]
   (let [coordinates (parse input)]
     (->> coordinates
          horizontal-and-vertical
          dups
          count))))

(defn- vertical? [[[x1 y1] [x2 y2]]]
  (=
    (- (max x1 x2) (min x1 x2))
    (- (max y1 y2) (min y1 y2))))

(defn vertical [coordinates]
  (->> coordinates
       (filter vertical?)
       (mapcat (fn [[[x1 y1] [x2 y2]]]
         (map (fn [x y]
             [x y])
           (cond->> (range (min x1 x2) (inc (max x1 x2)))
             (> x1 x2) reverse)
           (cond->> (range (min y1 y2) (inc (max y1 y2)))
             (> y1 y2) reverse))))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day5/input.txt")))
  ([input]
   (let [coordinates (parse input)]
     (->> (concat
            (horizontal-and-vertical coordinates)
            (vertical coordinates))
          dups
          count))))


(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
