(ns advent_2018.seven.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example 
"Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defn parse-line [line]
  (let [[[_ step before-step]] (re-seq #"Step ([A-Z]).* step ([A-Z])" line)]
    [(keyword step) (keyword before-step)]))

(defn parse [lines]
  (let [lines (s/split-lines lines)
        [start _] (parse-line (first lines))]
    (loop [lines lines
           order {}]

      (if (empty? lines)
        [start order]
        (let [[step before-step] (parse-line (first lines))]
          (recur
            (next lines)
            (update order step #(conj % before-step))))))))

(defn- ready? [requirements order instruction]
  (let [requirements-left (flatten (vals (apply dissoc requirements (set order))))]
    (when-not (some #{instruction} requirements-left)
      instruction)))



(defn part-one
  ([] (part-one (slurp "./src/2018/day7/input.txt")))
  ([input]
   (let [[start requirements] (parse input)]
     (loop [instructions (sort (start requirements))
            order [start]]

       (if (empty? instructions)
         order
         (let [instr (some #(ready? requirements order %) instructions)
               new-order (conj order instr)]
           (recur
             (->> (concat instructions (instr requirements))
                  (remove (set new-order))
                  sort)
             new-order)))))))

(comment
(part-one example)
(part-one))
