(ns advent-of-code-2021.core
   (:require [clojure.string :as string]
             [clojure.set :as s]))

(def example 
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
")

(defn- overlapping [[l1 l2]]
  (into [] (s/intersection (set l1) (set l2))))

(defn- overlapping' [[l1 l2 l3]]
  (into [] (s/intersection (set l1) (set l2) (set l3))))

(def priorities
  (zipmap (concat (map char (range (int \a) (inc (int \z))))
                  (map char (range (int \A) (inc (int \Z)))))
          (range 1 57)))

(defn- parse-compartments [input]
  (->> (string/split-lines input)
       (map (fn [s]
              [(apply str (take (/ (count s) 2) s))
               (apply str (drop (/ (count s) 2) s))]))))

(defn- parse-bags [input]
  (->> (string/split-lines input)
       (partition 3)))

(defn part-one
  ([]  (part-one (slurp "./src/2022/day3/input.txt")))
  ([input]
   (->> (parse-compartments input)
        (mapcat overlapping)
        (map #(get priorities %))
        (reduce +))))

(defn part-two
  ([]  (part-two (slurp "./src/2022/day3/input.txt")))
  ([input]
   (->> (parse-bags input)
        (mapcat overlapping')
        (map #(get priorities %))
        (reduce +))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
