(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.data.priority-map :refer [priority-map]]))

(def example "target area: x=20..30, y=-10..-5")

(defn- parse [input]
 (->> (re-seq #"-*\d*\.\.-*\d*" input)
  (map #(string/split % #"\.\." ))
  (map (fn [row]
         (map #(Integer/parseInt %) row))))) 

(->> (parse example)
     (map (fn [row]
            (map #(Integer/parseInt %) row))))

(range 20 30)
(some 11 (range -5 10))


(defn- apply-drag [x-velocity]
  (cond 
    (zero? x-velocity) x-velocity
    (neg? x-velocity) (inc x-velocity)
    (pos? x-velocity) (dec x-velocity)))

(defn- within-target-area? [[x y] x-range y-range]
  (and
    (some #{x} x-range)
    (some #{y} y-range)))

(defn- missed-target-area? [[x y] x-range y-range y-velocity]
  (or
    (> x (apply max x-range))
    (and
      (neg? y-velocity)
      (< y (apply min y-range)))))

(defn- shoot [[[x-min x-max] [y-min y-max]] [x-velocity y-velocity]]
  (let [x-range (range x-min (inc x-max))
        y-range (range y-min (inc y-max))]

    ;; (println :x-range x-range) (println :y-range y-range)

  (loop [[x y] [0 0]
         x-velocity x-velocity
         y-velocity y-velocity
         y-max 0]
    ;;
    ;; (println [x y])
    ;; (println :x-velocity x-velocity)
    ;; (println :y-velocity y-velocity)
    ;;
    (cond
      (within-target-area? [x y] x-range y-range) y-max
      (missed-target-area? [x y] x-range y-range y-velocity) nil
      :else (recur [(+ x x-velocity) (+ y y-velocity)]
                   (apply-drag x-velocity)
                   (dec y-velocity)
                   (max y-max y))))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day17/input.txt")))
  ([input]
   (let [target-area (parse input)
         shots (for [x (range 0 (-> target-area first second inc))
                     y (range 0 450)]
                 (shoot target-area [x y]))]


     (apply max (filter some? shots)))))

(comment
(parse example)
(part-one example)
(part-one))
