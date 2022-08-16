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
  (let [[_ step before-step] (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." line) ]
    [step before-step]))

(defn- parse [input]
  (let [lines (map parse-line (s/split-lines input))
        steps (set (concat (map first lines) (map second lines)))]
    (merge
      (into {} (->> steps (map (fn [s]
                                 [s #{}])) (into {})))
      (reduce (fn [a [before after]]
                (assoc a after (conj (a after (sorted-set)) before)))
              {}
              lines))))

(defn part-one
  ([] (part-one (slurp "./src/2018/day7/input.txt")))
  ([input]
   (let [requirements (parse input)]
     (loop [requirements requirements
            order ""]
       (if (empty? requirements)
         (s/join order)
         (let [next-instruction (->> requirements
                                     (filter #(empty? (second %)))
                                     (map first)
                                     sort
                                     first)
               requirements' (into {} (map (fn [[k v]]
                                             [k (disj v next-instruction)])
                                           requirements))]
           (recur
             (dissoc requirements' next-instruction)
             (str order next-instruction))))))))
                 
(defn- ready? [requirements order instruction]
  (when (cs/superset? (set order) (instruction requirements))
    instruction))

(def step-time
  (reduce (fn [a i]
            (assoc a (keyword (str (char (+ (int \A) i)))) (inc i)))
          {}
          (range 0 26)))

(defn- task-time [node]
  (+ 60 (step-time node)))

(defn- worker-ready? [[_ time-left]]
  (zero? time-left))

(defn- find-start [requirements all-keys]
  (->> all-keys
       set
       (remove #(contains? requirements %))
       sort))
             
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
