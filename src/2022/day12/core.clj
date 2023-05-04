(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(add-tap println)

(def example
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
")

(defn- at-most-one-higher? [current step]
  (or (>= (int current) (int step))
      (>= 1 (abs (- (int current) (int step))))))

(defn- parse [input]
  (mapv #(mapv identity %) (string/split-lines input)))

(defn- square [area [x y]]
  (nth (nth area y nil) x nil))

(defn- find-square [area value]
  (first (for [x (range 0 (-> area first count))
               y (range 0 (count area))
               :when (= value (square area [x y]))]
           [x y])))

(def visited {}

(defn walk [area path]
  (let [position (last path)
        [x y] position
        current-square (square area position)
        right [(inc x) y]
        left [(dec x) y]
        up [x (inc y)]
        down [x (dec y)]
        new-paths (->> [right left up down]
                       (remove #(get (set path) %))
                       (filter (partial square area)))]

    ;; (tap> [:new-paths new-paths])

    (->> new-paths
         (keep (fn [coords]
                 (when (at-most-one-higher? current-square (square area coords))
                   coords)))
         (map (fn [chord]
                (conj path chord))))))

(defn part-one
  ([] (part-one (slurp "./src/2022/day12/input.txt")))
  ([input]
   (let [area (parse input)
         start (find-square area \S)
         end (find-square area \E)
         area' (-> area
                   (assoc-in (reverse start) \a)
                   (assoc-in (reverse end)   \z))]

     (loop [steps 0
            paths [[start]]]

       (if (some #(when (= (square area (last %)) \E) %) paths)
         [:DONE steps]
         (if (< steps 50)
           (recur (inc steps)
                  (mapcat #(walk area' %) paths))
           [:nope])))
     )))


(comment
(part-one example)
(part-one ))
