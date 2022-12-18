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

;; overlapping' :: Ord a => [[a]] -> [a]
;; overlapping' (l1: l2: l3:_) = Set.toList $ Set.intersection(Set.fromList l1) $ Set.intersection (Set.fromList l2) (Set.fromList l3)

;; part2 :: String -> Int
;; part2 input = sumPriorities $ map overlapping' $ chunksOf 3 $ lines input

(defn- overlapping [[l1 l2]]
  (into [] (s/intersection (set l1) (set l2))))

(def priorities
  (zipmap (concat (map char (range (int \a) (inc (int \z))))
                  (map char (range (int \A) (inc (int \Z)))))
          (range 1 57)))

(defn- parse [input]
  (->> (string/split-lines input)
       (map (fn [s]
              [(apply str (take (/ (count s) 2) s))
               (apply str (drop (/ (count s) 2) s))]))))

(parse example)
(map overlapping (parse example))

(defn part-one
  ([]  (part-one (slurp "./src/2022/day2/input.txt")))
  ([input]
   (->> (parse input)
        (mapcat overlapping)
        (map #(get priorities %))
        (reduce +))))

(defn part-two
  ([]  (part-two (slurp "./src/2022/day2/input.txt")))
  ([input]
   (apply + (map play-v2 (parse input)))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

