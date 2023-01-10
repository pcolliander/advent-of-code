(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(defonce p (p/open))
(add-tap #'p/submit)

(def example
"30373
25512
65332
33549
35390")

(defn transpose [m]
  (apply mapv vector m))

(defn parse [input] 
  (->> (string/split-lines input)
    (map (comp
           #(map parse-long %)
           #(string/split % #"")))))

(defn- line-of-sight [tree-line n]
  (let [before (take n tree-line)
        after  (drop (inc n) tree-line)]
    [before after]))

(defn part-one
  ([] (part-one (slurp "./src/2022/day8/input.txt")))
  ([input]
   (let [trees (parse input)]
     (->> (for [row    (range 0 (count trees))
                column (range 0 (count (nth trees row)))
                :let [tree-line-h (nth trees row)
                      tree-line-v (nth (transpose trees) column)
                      tree (nth tree-line-h column)
                      [before after] (line-of-sight tree-line-h column)
                      [above under]  (line-of-sight tree-line-v row)]
                      :when (or
                              (every? #(> tree %) after)
                              (every? #(> tree %) before)
                              (every? #(> tree %) under)
                              (every? #(> tree %) above))]
            tree)
          count))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))

