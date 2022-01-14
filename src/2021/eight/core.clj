(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")


(defn- parse [input-path]
  (->> (string/split (slurp input-path) #",")
       (map string/trim)
       (map #(Integer/parseInt %))))


(defn part-one
  ([] (part-one "./seven/test-input.txt"))
  ([input-path]
   (let [positions (parse input-path)
         costs (for [align-to (range (apply min positions) (inc (apply max positions)))]
                 (cost positions align-to))]

     (apply min costs))))


(comment
(part-one  "./seven/input.txt")
(part-two  "./seven/input.txt"))
