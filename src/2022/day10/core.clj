(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(defonce p (p/open))
(add-tap #'p/submit)

(def example
"noop
addx 3
addx -5")

(defn parse [input] 
  (flatten (map  (fn [x]
          (let [[_ m] (re-find #"noop|addx (-*\d+)" x)]
            (if m
              [:noop (parse-long m)]
              :noop)))
  (string/split-lines input))))

(def cycles [20 60 100 140 180 220])

(defn part-one
  ([] (part-one (slurp "./src/2022/day10/input.txt")))
  ([input]
   (let [instructions (parse input)]
     (loop [cycle-n 1
            x 1
            execution [(first instructions)]
            instructions (next instructions)
            signal-strength 0]

       (let [signal-strength' (if (some #{cycle-n} cycles)
                                (+ signal-strength (* cycle-n x))
                                signal-strength)]
         (if-let [ex (first execution)]
           (let [[instruction & instructions'] instructions]
               (recur (inc cycle-n)
                      (if (= :noop ex) x (+ x ex))
                      (if instruction (conj (next execution) instruction) (next execution))
                      instructions'
                      signal-strength'))
           signal-strength))))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day9/input.txt")))
  ([input]
   (let [motions (parse input)])))

(comment
  (part-one example)
  (part-one (slurp "./src/2022/day10/large-example.txt"))
  (part-one)
  (part-two example)
  (part-two large-example)
  (part-two))
