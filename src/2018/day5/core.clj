(ns advent_2018.five.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example "dabAcCaCBAcCcaDA")

(defn- unit-polarity? [u1 u2]
  (let [lower-case? (= (str u1) (s/lower-case u1))]
    (if lower-case?
      (= (s/upper-case u1) (str u2))
      (= (s/lower-case u1) (str u2)))))

(defn- reduce-one [string]
  (loop [n 0 result []]
    (if-let [unit (nth string n nil)]
      (if-let [next-unit (nth string (inc n) nil)]
        (if (unit-polarity? unit next-unit)
          (recur (+ n 2) result)
          (recur (inc n) (conj result unit)))
        (conj result unit)))))

(defn part-one
  ([] (part-one (slurp "./src/2018/day5/input.txt")))
  ([input]
   (loop [prev input]
     (let [result (reduce-one prev)]
       (if (= result prev)
         (count (s/join result))
         (recur result ))))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day5/input.txt")))
  ([input]))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
