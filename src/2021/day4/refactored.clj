(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

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

(defn parse [input-path]
  (let [[order & boards] (string/split-lines (slurp input-path))
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
  ([] (part-one "./four/test-input.txt"))
  ([input-path]
   (let [{:keys [order boards]} (parse input-path)]
     (pp/pprint order)
     (println)
     (pp/pprint boards)

     (loop [n 1]
       (let [nums (take n order)]
         (or
           (some #(score % nums) boards)
           (recur (inc n))))))))

(defn part-two
  ([] (part-two "./four/test-input.txt"))
  ([input-path]
   (let [{:keys [order boards]} (parse input-path)]
     (loop [n 1 boards boards]
       (let [nums (take n order)
             boards' (remove #(score % nums) boards)]
         (if (empty? boards')
           (score (last boards) nums)
           (recur (inc n) boards')))))))
(comment
  (part-one "./four/input.txt")
  (part-two "./four/input.txt"))
