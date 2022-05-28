(ns advent-of-code-2018.core
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(defn- parse [input]
  (->> (s/split-lines input)
       (map parse-long)))

(defn part-one
  ([] (part-one (slurp "./src/2018/day1/input2.txt")))
  ([input]
   (reduce + (parse input))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day1/input.txt")))
  ([input]
   (loop [values (cycle (parse input))
          seen #{}
          current-freq 0]
     (if-let [[head & tail] values]
       (let [new-freq (+ head current-freq)]
         (if (contains? seen new-freq)
           new-freq
           (recur
             tail
             (conj seen new-freq)
             new-freq)))))))

(comment
  (part-one)
  (part-two))
