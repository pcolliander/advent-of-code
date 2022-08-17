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
                 
(def step-time
  (reduce (fn [a i]
            (assoc a (str (char (+ (int \A) i))) (+ 60 (inc i))))
          {}
          (range 0 26)))

(defn part-two
  ([] (part-two (slurp "./src/2018/day7/input.txt") 5))
  ([input workn]
   (loop [requirements (parse input)
          seconds 0
          workers []]
     (let [{finished false active true} (->> (for [[letter time-left] workers]
                                               [letter (dec time-left)])
                                             (group-by (comp pos? second)))
           requirements' (->> (apply dissoc requirements (map first finished))
                              (map (fn [[k v]]
                                     [k (apply disj v (map first finished))]))
                              (into {}))
           available-requirements (->> requirements'
                                       (remove (fn [[instruction reqs]]
                                                 (or
                                                   (seq reqs)
                                                   (some #{instruction} (map first active)))))
                                       (map first))
           workers' (reduce (fn [a req]
                              (if (>= (count a) workn)
                                (reduced a)
                                (conj a [req (step-time req)])))
                            active
                            available-requirements)]
       (if (empty? requirements')
         seconds
         (recur
           requirements'
           (inc seconds)
           workers'))))))

(comment
(part-one example)
(part-one)
(part-two example 2)
(part-two))
