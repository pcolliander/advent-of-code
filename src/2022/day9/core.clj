(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(defonce p (p/open))
(add-tap #'p/submit)

(def example
"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def large-example
"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn parse [input] 
  (map (comp
         (fn [[l n]]
           [(keyword l) (parse-long n)])
         #(string/split % #" "))
       (string/split-lines input)))

(defn- adjacent? [[x1 y1] [x2 y2]]
  (and
    (> 2 (abs (- x2 x1)))
    (> 2 (abs (- y2 y1)))))

(defn part-one
  ([] (part-one (slurp "./src/2022/day9/input.txt")))
  ([input]
   (let [motions (parse input)]
     (loop [knot [[0 0] [0 0]]
            tail-visited #{[0 0]}
            motions motions]
       (if-let [[direction quantity] (first motions)]
         (let [[knot' visited] (reduce (fn [[[head tail] visited] step]
                                         (let [head' (case step
                                                       :R [(inc (first head)) (second head)]
                                                       :L [(dec (first head)) (second head)]
                                                       :U [(first head) (inc (second head))]
                                                       :D [(first head) (dec (second head))])
                                               tail' (if (adjacent? head' tail) tail head)]
                                           [[head' tail'] (conj visited tail')]))
                                       [knot #{}]
                                       (repeat quantity direction))]
           (recur knot' (apply conj tail-visited (vec visited)) (next motions)))
         (count tail-visited))))))

(defn- move-diagonally [[x1 y1] pos2]
  (let [up-right [(inc x1) (inc y1)]
        down-right [(inc x1) (dec y1)]
        left-up [(dec x1) (inc y1)]
        left-down [(dec x1) (dec y1)]]
    (some #(when (adjacent? pos2 %) %) [up-right down-right left-up left-down])))

(defn- move-vertically-or-horizontally [[x1 y1] pos2]
  (let [up    [x1 (inc y1)]
        down  [x1 (dec y1)]
        left  [(dec x1) y1]
        right [(inc x1) y1]]
    (some #(when (adjacent? pos2 %) %) [up down left right])))

(defn- move [[x1 y1] [x2 y2]]
  (if (and
        (not= x1 x2)
        (not= y1 y2))
    (move-diagonally [x1 y1] [x2 y2])
    (move-vertically-or-horizontally [x1 y1] [x2 y2])))

(defn part-two
  ([] (part-two (slurp "./src/2022/day9/input.txt")))
  ([input]
   (let [motions (parse input)]
     (loop [knot (repeat 10 [0 0])
            tail-visited #{[0 0]}
            motions motions]
       (if-let [[direction quantity] (first motions)]
         (let [[knot' visited] (reduce (fn [[[head & tail] visited] step]
                                         (let [head' (case step
                                                       :R [(inc (first head)) (second head)]
                                                       :L [(dec (first head)) (second head)]
                                                       :U [(first head) (inc (second head))]
                                                       :D [(first head) (dec (second head))])
                                               knot' (reduce (fn [acc n]
                                                               (let [h (last acc)]
                                                                 (if (adjacent? h n)
                                                                   (conj acc n)
                                                                   (conj acc (move n h)))))
                                                             [head']
                                                             tail)]
                                           [knot' (conj visited (last knot'))]))
                                       [knot #{}]
                                       (repeat quantity direction))]
           (recur knot' (apply conj tail-visited (vec visited)) (next motions)))
         (count tail-visited))))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two large-example)
  (part-two))

