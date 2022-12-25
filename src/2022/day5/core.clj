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

(def example'
"[P]     [L]         [T]            
[L]     [M] [G]     [G]     [S]    
[M]     [Q] [W]     [H] [R] [G]    
[N]     [F] [M]     [D] [V] [R] [N]
[W]     [G] [Q] [P] [J] [F] [M] [C]
[V] [H] [B] [F] [H] [M] [B] [H] [B]
[B] [Q] [D] [T] [T] [B] [N] [L] [D]
[H] [M] [N] [Z] [M] [C] [M] [P] [P]
 1   2   3   4   5   6   7   8   9

move 1 from 2 to 1
move 3 from 1 to 3")

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
   (let [[stacks instructions] (parse' input)
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

(comment
  (part-one example)
  (part-one)
  (part-two)
  (part-two))
