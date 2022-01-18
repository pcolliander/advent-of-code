(ns advent-of-code-2021.core
   (:require [clojure.string :as string]))

(def example "199
             200
             208
             210
             200
             207
             240
             269
             260
             263")

(defn- count-increases [xs]
  (->> (map - xs (next xs))
       (filter neg?)
       (count)))

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map #(Integer/parseInt %))))

(defn part-one
  ([]  (part-one (slurp "./src/2021/day1/input.txt")))
  ([input]
   (count-increases (parse input))))

(defn part-two
  ([]  (part-two (slurp "./src/2021/day1/input.txt")))
  ([input]
  (let [lines (parse input)
        xs (map + lines (next lines) (nnext lines))]
    (count-increases xs))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

