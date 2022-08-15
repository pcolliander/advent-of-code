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
    [step before-step]))

(defn- parse-requirements [lines]
  (let [steps (set (concat (map first lines) (map second lines)))]
    (merge
      (into {} (->> steps (map (fn [s]
                                 [s #{}])) (into {})))
      (reduce (fn [a [before after]]
                (assoc a after (conj (a after (sorted-set)) before)))
              {}
              lines))))

(parse-requirements (map parse-line (s/split-lines example)))

(defn- find-start [requirements all-keys]
  (->> all-keys
       set
       (remove #(contains? requirements %))
       sort))

(defn part-one
  ([] (part-one (slurp "./src/2018/day7/input.txt")))
  ([input]
   (let [lines (map parse-line (s/split-lines input))
         requirements (parse-requirements lines)]
     (loop [requirements requirements
            order []]
       (println :requirements requirements)
       (println :order order)
       (if (empty? requirements)
         (s/join order)
         (let [requirements' (into {} (map (fn [[k v]]
                                             [k (disj v (last order))])
                                           requirements))
               open (->> requirements' (filter #(empty? (second %))) (map first))
               next-item (first (sort open))]
           (recur
             (dissoc requirements' next-item)
             (conj order next-item))))))))

(comment
  (let [input #_(slurp "./src/2018/day7/input.txt") example
        lines (map parse-line (s/split-lines input))
        steps (set (concat (map first lines) (map second lines)))
        reqs (merge
               (->> steps (map (fn [s]
                                 [s #{}])) (into {}))
               (reduce (fn [a [before after]]
                         (assoc a after (conj (a after (sorted-set)) before)))
                       {}
                       lines))]
    (loop [reqs reqs
           res ""]
      (if (empty? reqs)
        res
        (let [open (->> reqs (filter #(empty? (second %))) (map first))
              go-with (first (sort open))
              reqs' (into {}
                          (for [[k vals] reqs]
                            [k (disj vals go-with)]))]
          (recur (dissoc reqs' go-with) (str res go-with)))))))


(comment
(part-one example)
(part-one)
)
                 
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
