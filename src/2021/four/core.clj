(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(defn- calculate-score [rows drawn-numbers last-number]
  (->> (flatten rows)
    (remove drawn-numbers)
    (map (fn [numb]
           (Integer/parseInt numb)))
    (reduce + )
    (* (Integer/parseInt last-number))))

(defn part-one
  ([] (part-one "./four/test-input.txt"))
  ([input-path]
   (let [lines (string/split-lines (slurp input-path))
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
  ([] (part-two "./four/test-input.txt"))
  ([input-path]
   (let [lines (string/split-lines (slurp input-path))
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

             ; wait til all boards are matched.

             ; when the last one matches, do the calculation.

             ;; (println :matched? matched?)
             (println :last-board-matched? last-board-matched?)
             ;; (println :matched? matched?)
             (println :won-boards won-boards)
             
             
             (cond
               last-board-matched? (do
                                     (calculate-score rows drawn-numbers last-number))
               matched? (recur (next boards) (inc won-boards))
               :else    (recur (next boards) won-boards)))
           nil))))))

(defn part-two-second-try
  ([] (part-two-second-try "./four/test-input.txt"))
  ([input-path]
   (let [lines (string/split-lines (slurp input-path))
         draw-order (-> (first lines) (string/split #","))
         all-boards (->> (next lines)
                     (remove string/blank?)
                     (partition 5))]

     (loop [matched-boards []
            boards all-boards
            pos 0]
       (if-let [board (first boards)]
         (let [drawn-numbers (-> (take pos draw-order) set)
               rows (->> board 
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
               
               last-board-unmatched? (= 1 (- (count all-boards) (count matched-boards)))]

           (cond
             (and matched? last-board-unmatched?) :all-done
             matched? (recur (concat matched-boards board) (next boards) (inc pos)))))))))

(comment
  (part-one  #_"./four/input.txt")
  (part-two  #_"./four/input.txt")
  (part-two-second-try  #_"./four/input.txt"))
