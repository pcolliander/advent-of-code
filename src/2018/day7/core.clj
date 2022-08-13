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

(defn- parse [lines]
  (let [lines (s/split-lines lines)]
    (loop [lines lines
           requirements {}
           graph {}]
      (if (empty? lines)
        [graph requirements]
        (let [[before after] (parse-line (first lines))]

          (recur
            (next lines)
            (assoc requirements after (conj (requirements after (sorted-set)) before))
            (assoc graph before (conj (graph before (sorted-set)) after))))))))

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
         start-nodes (find-start graph requirements)
         [start-node & r] start-nodes]
     (loop [instructions (apply sorted-set (concat (start-node graph) r))
            order [start-node]]
       (if (empty? instructions)
         (s/join (map name order))
         (let [instruction (some #(ready? requirements order %) instructions)
               new-order (conj order instruction)]
           (recur
             (cs/difference (apply conj instructions (instruction graph)) (set new-order))
             new-order)))))))

(def step-time
  (reduce (fn [a i]
            (assoc a (keyword (str (char (+ (int \A) i)))) (inc i)))
          {}
          (range 0 26)))

(defn- task-time [node]
  (+ 60 (step-time node)))

(defn- worker-ready? [[_ time-left]]
  (zero? time-left))

(defn part-two
  ([] (part-two (slurp "./src/2018/day7/input.txt") 5))
  ([input workn]
   (let [[graph requirements] (parse input)
         start-nodes (find-start graph requirements)
         workers (map #(vector % (task-time %)) start-nodes)]
     (loop [instructions (sorted-set)
            order []
            seconds 0
            workers workers]
       (if (and (empty? instructions) (zero? (count workers)))
         [seconds (s/join (map name order))]
         (let [workers' (map (fn [[letter time-left]]
                                [letter (dec time-left)]) workers)
               finished-workers (filter worker-ready? workers')
               new-order (concat order (map first finished-workers))
               new-instructions (apply conj instructions (cs/difference (set (mapcat (fn [[instruction _]]
                                                                                       (instruction graph)) finished-workers))
                                                                        (set new-order)))
               workers'' (reduce (fn [a instruction]
                                   (let [ready (ready? requirements new-order instruction)]
                                     (if (and ready (< (count a) workn))
                                       (conj a [ready (task-time ready)])
                                       a)))
                                 (filterv (complement worker-ready?) workers')
                                 new-instructions)]
           (recur
             (cs/difference new-instructions (set (map first workers'')))
             new-order
             (inc seconds)
             workers'')))))))

(comment
(part-one example)
(part-one)
(part-two example 2)
(part-two))
