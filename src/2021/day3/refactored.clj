(ns advent-of-code-2021.refactored
  (:require [clojure.edn :as edn]
             [clojure.string :as string]))

(defn- fqs [lines pos]
  (let [digits (map #(nth % pos) lines)
        {zeros \0 ones \1 :or {zeros 0 ones 0}} (frequencies digits)]
   [zeros ones]))

(defn part-one
  ([] (part-one "./three/test-input.txt"))
  ([input-path]
   (let [lines (string/split-lines (slurp input-path))
         digits (for [pos (range (count (first lines)))
                      :let [[zeros ones] (fqs lines pos)]]
                  (if (> zeros ones) 0 1))
         gamma (Integer/parseInt (string/join digits) 2)
         epsilon (-> (map #(- 1 %) digits) string/join (Integer/parseInt 2))]
     (* gamma epsilon))))
 
; PART 2
(defn part-two
  ([] (part-two "./three/test-input.txt"))
  ([input-path]
   (let [lines (string/split-lines (slurp input-path))
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

;; (+ 1 1)
(comment
  (part-one "./three/input.txt")
  (part-two "./three/input.txt"))
