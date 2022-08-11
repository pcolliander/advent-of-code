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
  (let [lines (s/split-lines lines)]
    (loop [lines lines
           requirements {}
           graph {}]

      (if (empty? lines)
        [graph requirements]
        (let [[requirement step] (parse-line (first lines))]
          (recur
            (next lines)
            (update requirements step #(conj % requirement))
            (update graph requirement #(conj % step))))))))

(defn- find-start [graph requirements]
  (->> (keys graph)
       (remove (fn [k]
                 (contains? requirements k)))
       sort))

(defn- ready? [requirements order instruction]
  (when (cs/superset? (set order) (instruction requirements))
    instruction))

(defn part-one
  ([] (part-one (slurp "./src/2018/day7/input.txt")))
  ([input]
   (let [[graph requirements] (parse input)
         start-nodes (sort (find-start graph requirements))
         [start-node & r] start-nodes]
     (loop [instructions (sort (concat (start-node graph) r))
            order [start-node]]
       (if (empty? instructions)
         (s/join (map name order))
         (let [instr (some #(ready? requirements order %) instructions)
               new-order (conj order instr)]
           (recur
             (->> (concat instructions (instr graph))
                  (remove (set new-order))
                  sort)
             new-order)))))))

(comment
(part-one example)
(part-one))
