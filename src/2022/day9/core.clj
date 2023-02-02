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
                                           [[head' (if (adjacent? head' tail ) tail head)]
                                            (conj visited tail')]))
                                       [knot #{}]
                                       (repeat quantity direction))]
           (recur knot' (apply conj tail-visited (vec visited)) (next motions)))
         (count tail-visited))))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day8/input.txt")))
  ([input]
   (let [steps (parse input)]
     )))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))

