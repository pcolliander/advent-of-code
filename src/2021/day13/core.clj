(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.set :as cset]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "6,10
             0,14
             9,10
             0,3
             10,4
             4,11
             6,0
             6,12
             4,1
             0,13
             10,12
             3,4
             3,0
             8,4
             1,10
             2,14
             8,10
             9,0

             fold along y=7
             fold along x=5")

(defn- parse [input]
  (let [fold-instructions (->> (re-seq #"fold along (x|y)=(\d+)" input)
                               (map (fn [[_ op value]]
                                      [(keyword op) (Integer/parseInt value)])))
        
        dots (->> (string/split-lines input)
                  (map string/trim)
                  (take-while (complement string/blank?))
                  (map #(string/split % #","))
                  (map (fn [row]
                         (mapv #(Integer/parseInt %) row))))]
    {:fold-instructions fold-instructions
     :paper dots}))

(defn- fold-up [fold-y [x y]]
  (let [distance-from-fold (- y fold-y)
        new-y (- fold-y distance-from-fold)]

    (if (< fold-y y)
      [x new-y]
      [x y])))

(defn- fold-left [fold-x [x y]]
  (let [distance-from-fold (- x fold-x)
        new-x (- fold-x distance-from-fold)]

    (if (< fold-x x)
      [new-x y]
      [x y])))

(defn- fold [[op value] coordinates]
  (case op
    :x (fold-left value coordinates)
    :y (fold-up value coordinates)))

(defn fold-paper [instructions paper]
  (loop [instructions instructions
         paper paper]
    (if-let [instruction (first instructions)]
      (recur (next instructions)
             (map #(fold instruction %) paper))
      (set paper))))

(defn part-one
  ([] (part-one (slurp "./thirteen/input.txt")))
  ([input]
   (let [{:keys [paper fold-instructions]} (parse input)
         first-instruction (first fold-instructions)
         dots (fold-paper (vector first-instruction) paper)]
     (count dots))))

(defn- print-code [folded-paper]
  (let [max-x (->> folded-paper (map first) (apply max))
        max-y (->> folded-paper (map second) (apply max))]
   (dotimes [y (inc max-y)]
       (dotimes [x (inc max-x)]
         (if (folded-paper [x y])
           (print "█")
           (print "░")))
       (println))))

(defn- ugly-print [folded-paper]
  (let [max-x (->> folded-paper (map first) (apply max))
        max-y (->> folded-paper (map second) (apply max))
        code (for [y (range 0 (inc max-y))]
                (println (for [x (range 0 (inc max-x))]
                  (if (folded-paper [x y])
                    (print "█")
                    (print "░")))))]
    (pp/pprint code)))

(defn part-two
  ([] (part-two (slurp "./thirteen/input.txt")))
  ([input]
   (let [{:keys [paper fold-instructions]} (parse input)
         folded-paper (fold-paper fold-instructions paper)]

     #_(ugly-print folded-paper) 
     (print-code folded-paper))))

(comment
(parse example)
(parse (slurp "./thirteen/input.txt"))


(part-one example)
(part-one)

(part-two example)
(part-two))


