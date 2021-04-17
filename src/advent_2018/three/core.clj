(ns advent_2018.three.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(defn- read-input [input-path]
  (->> (s/split-lines (slurp input-path))))

; means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.
(defn- parse-claim [claim]
  "a claim has the form: #123 @ 3,2: 5x4"
  (let [[id _ left-and-top width-and-tall] (s/split claim #" ")
      [inches-from-left inches-from-top] (s/split left-and-top #",") 
      [inches-wide inches-tall] (s/split width-and-tall #"x")]

  {:id id
   :inches-from-left (read-string inches-from-left)
   :inches-from-top (read-string (s/replace inches-from-top  #":" ""))
   :inches-wide (read-string inches-wide)
   :inches-tall (read-string inches-tall)}))

(defn- get-range [{:keys [id inches-from-left inches-from-top inches-tall inches-wide]}]
  (->> (range inches-from-left (+ inches-from-left inches-wide))
    (map #(range (+ (* 10 %) inches-from-top) (+ (* 10 %)  inches-from-top inches-tall)))
    (flatten)
    (set))) 

; define ranges, and if ranges overlap, then save the overlapping numbers. each number is a square. 
(defn- get-overlap [left right]
  (clojure.set/intersection left right))

(defn part-one [input-path]
  (let [ranges (->> (read-input input-path)
                    (map parse-claim)
                    (map get-range))]
    
    (loop [ranges ranges
           overlap #{}]

      (if-let [[head & tail] ranges]
        (let [result (->> tail 
                       (map (partial get-overlap head))
                       (reduce clojure.set/union))]

          (recur tail (clojure.set/union overlap result)))

        (do
          ;; (println :overlap overlap)
          (println :result (count overlap)))))))

;; (part-one "./src/advent_2018/three/test-input.txt")
(part-one "./src/advent_2018/three/input.txt")

