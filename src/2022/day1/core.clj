(ns advent-of-code-2021.core
   (:require [clojure.string :as string]))

(def example 
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn- parse [input]
  (->> (string/split input #"\n\n")
       (map string/split-lines)
       (map #(map parse-long %))
       (map #(reduce + %))))

(defn part-one
  ([]  (part-one (slurp "./src/2022/day1/input.txt")))
  ([input]
   (apply max (parse input))))

(defn part-two
  ([]  (part-two (slurp "./src/2022/day1/input.txt")))
  ([input]
   (reduce + (take 3 (reverse (sort (parse input)))))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

