(ns advent_2018.day8
  (:require [clojure.string :as s]))

(def example "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn- parse-metadata [node]
  (let [[[child-n metadata-n] r] (split-at 2 node)]
    (if (zero? child-n)
      [(take metadata-n r)]
      (loop [children-left child-n
             remaining r
             metadata []]
        (if (zero? children-left)
          (conj metadata (take metadata-n remaining))
          (let [new-meta (parse-metadata remaining)
                length-of-children (reduce + (map (comp (partial + 2) count) new-meta))]
            (recur (dec children-left)
                   (drop length-of-children remaining)
                   (concat metadata new-meta))))))))

(defn part-one
  ([] (part-one (slurp "./src/2018/day8/input.txt")))
  ([input]
   (let [tree (->> (s/split input #" ")
                   (map parse-long))]
     (->> (parse-metadata tree)
          flatten
          (reduce +)))))

(defn- parse-node [node]
  (let [[[child-n metadata-n] r] (split-at 2 node)]
    (if (zero? child-n)
     [(+ 2 (count (take metadata-n r))) (reduce + (take metadata-n r))]
      (loop [children-left child-n
             remaining r
             children []]
        (if (zero? children-left)
          (let [parent-metadata (take metadata-n remaining)
                children-values (->> (map dec parent-metadata)
                                     (map (fn [meta-index]
                                            (let [[_ value] (nth children meta-index [0 0])]
                                              value))))
                length-of-node (+ 2 (->> children
                                         (map first)
                                         (reduce +)) (count parent-metadata))]

            [length-of-node (reduce + children-values)])
          (let [[metadata-count value] (parse-node remaining)]
            (recur (dec children-left)
                   (drop metadata-count remaining)
                   (conj children [metadata-count value]))))))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day8/input.txt")))
  ([input]
   (let [tree (->> (s/split input #" ")
                   (map parse-long))]
     (second (parse-node tree)))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

