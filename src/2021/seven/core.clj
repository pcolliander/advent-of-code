(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "16,1,2,0,4,2,7,1,2,14")

(defn- parse [input-path]
  (->> (string/split (slurp input-path) #",")
       (map string/trim)
       (map #(Integer/parseInt %))))

(defn cost [positions align-to]
  (->> positions
      (map #(Math/abs (- % align-to)))
      (reduce +)))

(defn part-one
  ([] (part-one "./seven/test-input.txt"))
  ([input-path]
   (let [positions (parse input-path)
         costs (for [align-to (range (apply min positions) (inc (apply max positions)))]
                 (cost positions align-to))]

     (apply min costs))))

;; PART TWO
(def cost-between-two-numbs (memoize (fn [difference]
  (let [fuel-costs (for [cost (range 1 (inc difference))]
                     cost)]
    (reduce + fuel-costs)))))

(defn incremental-cost [positions align-to]
  (->> positions
       (map (fn [step]
              (let [difference (Math/abs (- align-to step))]
                (cost-between-two-numbs difference))))
       (reduce +)))

(defn part-two
  ([] (part-two "./seven/test-input.txt"))
  ([input-path]
   (let [positions (parse input-path)
         costs (for [align-to (range (apply min positions) (inc (apply max positions)))]
                 (incremental-cost positions align-to))]
     (apply min costs))))

(comment
(part-one  "./seven/input.txt")
(part-two  "./seven/input.txt"))
