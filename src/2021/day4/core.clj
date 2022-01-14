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

(defn- calculate-score [rows drawn-numbers last-number]
  (->> (flatten rows)
    (remove drawn-numbers)
    (map (fn [numb]
           (Integer/parseInt numb)))
    (reduce + )
    (* (Integer/parseInt last-number))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day4/input.txt")))
  ([input]
   (let [lines (string/split-lines input)
         draw-order (-> (first lines) (string/split #","))
         boards (->> (next lines)
                     (remove string/blank?)
                     (partition 5))]
     (for [pos (range (count draw-order))
           :let [drawn-numbers (-> (take pos draw-order) set)
                 last-number (->> draw-order (take pos) last)]]
       (loop [boards boards]
         (if-let [board (first boards)]
           (let [rows (->> board 
                           (map (fn [row]
                                  (->> (string/split row #" ") (map string/trim) (remove string/blank?)))))
                 
                 columns (for [pos (range (count (first rows)))]
                               (map #(nth % pos) rows))
                 matched-rows (filter (fn [row]
                                        (every? drawn-numbers row)) rows)
                 matched-columns (filter (fn [column]
                                           (every? drawn-numbers column)) columns)
                 
                 matched?  (or 
                             (pos? (count matched-rows))   
                             (pos? (count matched-columns)))]

             (cond
               matched?  (calculate-score rows drawn-numbers last-number)
               :else (recur (next boards))))

           nil))))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day4/input.txt")))
  ([input]
   (let [lines (string/split-lines input)
         draw-order (-> (first lines) (string/split #","))
         all-boards (->> (next lines)
                     (remove string/blank?)
                     (partition 5))]
     (for [pos (range (count draw-order))
           :let [drawn-numbers (-> (take pos draw-order) set)
                 last-number (->> draw-order (take pos) last)]]
       (loop [boards all-boards
              won-boards 0]
         (if-let [board (first boards)]
           (let [rows (->> board 
                           (map (fn [row]
                                  (->> (string/split row #" ") (map string/trim) (remove string/blank?)))))
                 
                 columns (for [pos (range (count (first rows)))]
                               (map #(nth % pos) rows))
                 matched-rows (filter (fn [row]
                                        (every? drawn-numbers row)) rows)
                 matched-columns (filter (fn [column]
                                           (every? drawn-numbers column)) columns)
                 matched?  (or 
                             (pos? (count matched-rows))   
                             (pos? (count matched-columns)))
                 
                 last-board-left? (= 1 (- (count all-boards) won-boards))
                 last-board-matched? (and matched? last-board-left?)]
             (cond
               last-board-matched? (do
                                     (calculate-score rows drawn-numbers last-number))
               matched? (recur (next boards) (inc won-boards))
               :else    (recur (next boards) won-boards)))
           nil))))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
