(ns advent_2018.one.core
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(defn- read-input [input-path]
  (->> (s/split-lines (slurp input-path))
       (map edn/read-string)))

(defn part-one [input-path]
  (->> (read-input input-path)
       (reduce +)))

;; (part-one "./src/one/test-input.txt")
(part-one "./src/advent_2018/one/input2.txt")

(defn part-two
  ([input-path]
   (part-two (take 150000 (cycle (read-input input-path))) #{} 0))

  ([values seen current-freq]
   (if-let [head (first values)]
     (let [new-freq (+ head current-freq)]
       (if (contains? seen new-freq)
         new-freq
         (recur
           (rest values)
           (conj seen new-freq)
           new-freq))) )))

;; (part-two "./src/one/test-input.txt")
(part-two "./src/advent_2018/one/input2.txt")

