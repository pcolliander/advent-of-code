(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

             22 13 17 11  0
             8  2 23  4 24
             21  9 14 16  7
             6 10  3 18  5
             1 12 20 15 19

             3 15  0  2 22
             9 18 13 17  5
             19  8  7 25 23
             20 11 10 24  4
             14 21 16 12  6

             14 21 17 24  4
             10 16 15  9 19
             18  8 23 26 20
             22 11 13  6  5
             2  0 12  3  7")

(defn- score-rows [board drawn-numbers]
  (let [num-set (set drawn-numbers)]
    (when (some #(every? num-set %) board)
      (->> (flatten board)
           (remove num-set)
           (reduce +)
           (* (last drawn-numbers))))))

(defn- transpose [board]
  (apply map vector board))

(defn score [board drawn-numbers]
  (or 
    (score-rows board drawn-numbers)
    (score-rows (transpose board) drawn-numbers)))

(defn parse [input]
  (let [[order & boards] (string/split-lines input)
        parsed-order (-> order
                         (string/split #",")
                         (->> (mapv string/trim) (mapv #(Integer/parseInt %))))
        parsed-boards (->> boards
                          (remove string/blank?)
                          (map (fn [line]
                                 (->> (string/split line #" ")
                                      (map string/trim)
                                      (remove string/blank?)
                                      (map #(Integer/parseInt %)))))
                          (partition 5))]
    {:order parsed-order :boards parsed-boards}))

(defn part-one
  ([] (part-one (slurp "./src/2021/day4/input.txt")))
  ([input]
   (let [{:keys [order boards]} (parse input)]
     (pp/pprint order)
     (println)
     (pp/pprint boards)

     (loop [n 1]
       (let [nums (take n order)]
         (or
           (some #(score % nums) boards)
           (recur (inc n))))))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day4/input.txt")))
  ([input]
   (let [{:keys [order boards]} (parse input)]
     (loop [n 1 boards boards]
       (let [nums (take n order)
             boards' (remove #(score % nums) boards)]
         (if (empty? boards')
           (score (last boards) nums)
           (recur (inc n) boards')))))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
