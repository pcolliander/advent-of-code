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
            (assoc a (str (char (+ (int \A) i))) (inc i)))
          {}
          (range 0 26)))

(defn- task-time [node]
  (+ 0 60 (step-time node)))

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
   (loop [requirements (parse input)
          order ""
          seconds -1
          workers []]

     (if (empty? requirements)
       [seconds order]
       (let [workers' (map (fn [[letter time-left]]
                             [letter (dec time-left)]) workers)
             finished-requirements (->> workers'
                                        (filter worker-ready?)
                                        (map first))
             unfinished-workers (filter (complement worker-ready?) workers')
             requirements' (into {} (map (fn [[k v]]
                                           [k (apply disj v finished-requirements)])
                                         requirements))
             new-order (str order (s/join finished-requirements))
             available-requirements (cs/difference (->> requirements'
                                                        (filter #(empty? (second %)))
                                                        (map first)
                                                        set)
                                                   (set (map str new-order)))
             workers'' (reduce (fn [a req]
                                 (if (and
                                       (<= (count a) workn)
                                       (empty? (filter (fn [[l t]]
                                                         (= l req)) unfinished-workers)))
                                   (conj a [req (task-time req)])
                                   a))
                                 unfinished-workers
                                 available-requirements)]
         (recur
           (reduce (fn [a r]
                     (dissoc a r))
                   requirements' 
                   finished-requirements)
           new-order
           (inc seconds)
           workers''))))))

(comment
(part-one example)
(part-one)
(part-two example 2)
(part-two))
