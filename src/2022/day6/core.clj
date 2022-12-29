(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]))

(def example "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn parse [input] 
  (string/split input #""))

(defn- check-length
  ([letters length] (check-length letters length 0))
  ([letters length current-count]
   (let [package-length (count (set (take length letters)))]
     (if (== package-length length)
       (+ current-count length)
       (recur (next letters) length (inc current-count))))))

(defn part-one
  ([] (part-one (slurp "./src/2022/day6/input.txt")))
  ([input]
   (let [letters (parse input)]
     (check-length letters 4))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day6/input.txt")))
  ([input]
   (let [letters (parse input)]
     (check-length letters 14))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))
