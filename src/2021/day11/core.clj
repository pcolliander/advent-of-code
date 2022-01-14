(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.set :as cset]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "5483143223
              2745854711
              5264556173
              6141336146
              6357385478
              4167524645
              2176841721
              6882881134
              4846848554
              5283751526")

(def small-example "11111
                    19991
                    19191
                    19991
                    11111")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map (fn [row]
              (map (fn [number]
                 (Integer/parseInt (str number))) row)))))

(defn- value-at [octopuses [x y]]
  (nth (nth octopuses y) x))

(defn- neighbours [octopuses [x y]]
  (let [width (dec (count (first octopuses)))
        height (dec (count octopuses))]

  (cond-> []
    (pos? x) (conj [(dec x) y]) ; left
    (and (pos? x) (pos? y)) (conj [(dec x) (dec y)]) ; top-left
    (pos? y) (conj [x (dec y)]) ; top
    (and (pos? y) (< x width)) (conj [(inc x) (dec y)]) ; top-right
    (< x width) (conj [(inc x) y]) ; right
    (and (< x width) (< y height)) (conj [(inc x) (inc y)]) ; bottom-right
    (< y height) (conj [x (inc y)]) ; bottom
    (and (pos? x) (< y height)) (conj [(dec x) (inc y)])))) ; bottom-left

(defn- get-flashes
  ([octopuses] (get-flashes octopuses #{}))
  ([octopuses flashed]
   (let [flashes (->>
                   (for [x (range (count (first octopuses)))
                         y (range (count octopuses))

                         :let [value (value-at octopuses [x y])]
                         :when (< 9 value)]
                     [x y])
                   (remove flashed)
                   (distinct))]
     (when (seq flashes)
       flashes))))

(defn- reset-flashed [octopus]
  (if (< 9 octopus)
    0
    octopus))

(defn- increase-affected-neighbour [[x y] affected-neighbours octopus]
  ((fnil + 0) (get affected-neighbours [x y]) octopus))

(defn- step [{:keys [octopuses total-flashes] :or {total-flashes 0}}]
  (loop [octopuses (map #(map inc %) octopuses)
         flashed #{}]

    (if-let [flashes (get-flashes octopuses flashed)]
      (let [affected-neighbours (->> flashes
                                     (mapcat #(neighbours octopuses %))
                                     (frequencies))
            updated-octopuses (map-indexed (fn [y row]
                                             (map-indexed #(increase-affected-neighbour [%1 y] affected-neighbours %2) row)) octopuses)]

        (recur updated-octopuses (into flashed flashes)))

      {:octopuses (map #(map reset-flashed %) octopuses)
       :total-flashes (+ (count flashed) total-flashes)})))

(defn part-one
  ([] (part-one (slurp "./eleven/input.txt")))
  ([input]
   (let [octopuses (parse input)]
     (nth (iterate step {:octopuses octopuses}) 100))))

(defn part-two
  ([] (part-two (slurp "./eleven/input.txt")))
  ([input]
   (let [octopuses (parse input)]
     (loop [n 0]
       (let [{:keys [octopuses]} (nth (iterate step {:octopuses octopuses}) n)]
         (if (every? zero? (flatten octopuses))
           n
           (recur (inc n))))))))

(comment
  (nth (iterate step {:octopuses (parse small-example)}) 0)
  (nth (iterate step {:octopuses (parse small-example)}) 1)
  (nth (iterate step {:octopuses (parse small-example)}) 2)
  (nth (iterate step {:octopuses (parse example)}) 100)
  (nth (iterate step {:octopuses (parse example)}) 195)

  (part-one example)
  (part-one)
  (part-two example)
  (part-two))

