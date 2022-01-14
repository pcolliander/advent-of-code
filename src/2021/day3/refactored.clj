(ns advent-of-code-2021.refactored
  (:require [clojure.edn :as edn]
             [clojure.string :as string]))

(def example "00100
             11110
             10110
             10111
             10101
             01111
             00111
             11100
             10000
             11001
             00010
             01010")

(defn- fqs [lines pos]
  (let [digits (map #(nth % pos) lines)
        {zeros \0 ones \1 :or {zeros 0 ones 0}} (frequencies digits)]
   [zeros ones]))

(defn part-one
  ([]  (part-one (slurp "./src/2021/day3/input.txt")))
  ([input]
   (let [lines (string/split-lines input)
         digits (for [pos (range (count (first lines)))
                      :let [[zeros ones] (fqs lines pos)]]
                  (if (> zeros ones) 0 1))
         gamma (Integer/parseInt (string/join digits) 2)
         epsilon (-> (map #(- 1 %) digits) string/join (Integer/parseInt 2))]
     (* gamma epsilon))))
 
; PART 2
(defn part-two
  ([]  (part-two (slurp "./src/2021/day3/input.txt")))
  ([input]
   (let [lines (string/split-lines input)
         o2 (loop [lines lines pos 0]
              (if (<= (count lines) 1)
                (Integer/parseInt (first lines) 2)
                (let [[zeros ones] (fqs lines pos)]
                  (if (>= ones zeros)
                    (recur (filter #(= \1 (nth % pos)) lines) (inc pos))
                    (recur (filter #(= \0 (nth % pos)) lines) (inc pos))))))
         co2 (loop [lines lines pos 0]
               (if (<= (count lines) 1)
                 (Integer/parseInt (first lines) 2)
                 (let [[zeros ones] (fqs lines pos)]
                   (if (<= zeros ones)
                     (recur (filter #(= \0 (nth % pos)) lines) (inc pos))
                     (recur (filter #(= \1 (nth % pos)) lines) (inc pos))))))]
     (* o2 co2))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
