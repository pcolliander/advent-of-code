(ns advent-of-code-2021.core
   (:require [clojure.string :as string]
             [clojure.set :as s]))

(def example 
"A Y
B X
C Z
")

(def scores
  {:A 1
   :B 2
   :C 3})

(def equivalent
  {:X :A
   :Y :B
   :Z :C})

(def beats
  {:A :C
   :B :A
   :C :B})

(defn- play [[opponent you]]
  (let [response (you equivalent)]
    (cond
      (= opponent response) (+ (get scores response) 3)
      (= (get beats opponent) response) (get scores response)
      :else (+ (get scores response) 6))))

(defn- play-v2 [[opponent you]]
  (case you
    :X (+ (get scores (get beats opponent)))
    :Y (+ (get scores opponent) 3)
    :Z (+ (get scores (get (s/map-invert beats) opponent)) 6)))

(defn- parse [input]
  (->> (string/split-lines input)
       (map #(string/split % #" "))
       (map #(map keyword %))))

(defn part-one
  ([]  (part-one (slurp "./src/2022/day2/input.txt")))
  ([input]
   (apply + (map play (parse input)))))

(defn part-two
  ([]  (part-two (slurp "./src/2022/day2/input.txt")))
  ([input]
   (apply + (map play-v2 (parse input)))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

