(ns advent_2018.day8
  (:require [clojure.string :as s]))

(def example "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn- parse-node [node]
  (let [[[child-n metadata-n] r] (split-at 2 node)]
    (if (zero? child-n)
      [(take metadata-n r)]
      (loop [children-left child-n
             remaining r
             metadata []]
        (if (zero? children-left)
          (conj metadata (take metadata-n remaining))
          (let [new-meta (parse-node remaining)]
            (recur (dec children-left)
                   (drop (reduce (fn [a v]
                                   (+ a 2 (count v)))
                                 0
                                 new-meta)
                                 remaining)
                   (concat metadata new-meta))))))))

(defn part-one
  ([] (part-one (slurp "./src/2018/day8/input.txt")))
  ([input]
   (let [tree (->> (s/split input #" ")
                   (map parse-long))]
     (->> (parse-node tree)
          flatten
          (reduce +)))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
