(ns advent-of-code-2021.core
   (:require [clojure.string :as string]
             [clojure.set :as s]))

(def example 
"A Y
B X
C Z
")

(def scores
  {:X 1 ; rock
   :Y 2 ; paper
   :Z 3}) ;scissor

(def equivalent
  {:A :X
   :B :Y
   :C :Z})

(def beats
  {:A :Z
   :B :X
   :C :Y})

(def loses-to
  {:A :Y
   :B :Z
   :C :X})

(defn- play [[opponent you]]
  (cond
    (= (equivalent opponent) you) (+ (get scores you) 3)
    (= (get beats opponent) you) (get scores you)
    :else (+ (get scores you) 6)))

(defn- play-v2 [[opponent you]]
  (case you
    :X (+ (get scores (get beats opponent)))
    :Y (+ (get scores (get equivalent opponent)) 3)
    :Z (+ (get scores (get loses-to opponent)) 6)))

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

