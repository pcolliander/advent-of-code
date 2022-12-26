(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]))

(def example
"   [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn transpose [m]
  (apply mapv vector m))

(defn parse [input] 
  (let [[stacks, instructions] (string/split input #"\n\n")]
    [(->> (string/split-lines stacks)
         (butlast)
         (map #(map second (re-seq #"(?:\[(\w)\]| ( ) ) ?" %)))
         (transpose)
         (map #(filter some? %))
         (zipmap (drop-while zero? (range))))

     (->> (string/split-lines instructions)
          (map (fn [line]
                 (let [[match & matches] (re-matches #"move (\d*) from (\d) to (\d)" line)]
                       (map parse-long matches)))))]))

(defn part-one
  ([] (part-one (slurp "./src/2022/day5/input.txt")))
  ([input]
   (let [[stacks instructions] (parse input)
         with-applied-instructions (reduce (fn [acc [quantity from to]]
                                             (let [items (take quantity (get acc from))]
                                               (-> acc
                                                   (update from #(drop quantity %))
                                                   (update to #(apply conj % items)))))
                                           stacks
                                           instructions)]
     (->> (map vec with-applied-instructions)
          (sort-by first)
          (map (comp first second))
          (string/join "")))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day5/input.txt")))
  ([input]
   (let [[stacks instructions] (parse input)
         with-applied-instructions (reduce (fn [acc [quantity from to]]
                                             (let [items (take quantity (get acc from))]
                                               (-> acc
                                                   (update from #(drop quantity %))
                                                   (update to #(apply conj % (reverse items))))))
                                           stacks
                                           instructions)]

     (->> (map vec with-applied-instructions)
          (sort-by first)
          (map (comp first second))
          (string/join "")))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))
