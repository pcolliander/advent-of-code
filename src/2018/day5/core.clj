(ns advent_2018.five.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example "dabAcCaCBAcCcaDA")

(defn- unit-polarity? [^Character u1 ^Character u2]
  (if (Character/isLowerCase u1)
    (= (Character/toUpperCase u1) u2)
    (= (Character/toLowerCase u1) u2)))

(defn- react-once [string]
  (loop [n 0 result []]
    (if-let [unit (nth string n nil)]
      (if-let [next-unit (nth string (inc n) nil)]
        (if (unit-polarity? unit next-unit)
          (recur (+ n 2) result)
          (recur (inc n) (conj result unit)))
        (conj result unit)))))

(defn- react-full [string]
  (loop [prev string]
    (let [result (react-once prev)]
      (if (= result prev)
        result
        (recur result)))))

(defn part-one
  ([] (part-one (slurp "./src/2018/day5/input.txt")))
  ([input]
   (count (react-full input))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day5/input.txt")))
  ([input]
   (let [types (->> (set input) (map s/lower-case) set)]
     (->> (for [t types
                :let [purged (s/replace input (re-pattern (str "(?i)" t)) "")
                      reacted (react-full purged)]]
            [t (count reacted)])
          (sort-by second)))))

(comment
(part-one example)
(part-one) ; 2539
(part-two example)
(time (part-two))) ; (out) "Elapsed time: 32559.59491 msecs"
