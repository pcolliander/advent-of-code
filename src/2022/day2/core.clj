(ns advent-of-code-2021.core
   (:require [clojure.string :as string]))

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

(defn- play [[opponent you]]
  (cond
    (= (equivalent opponent) you) (+ (get scores you) 3)
    (= (get beats opponent) you) (get scores you)
    :else (+ (get scores you) 6)))

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
   (reduce + (take 3 (reverse (sort (parse input)))))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

