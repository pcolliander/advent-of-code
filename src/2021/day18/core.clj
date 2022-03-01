(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.zip :as zip]
            [clojure.data.priority-map :refer [priority-map]]))

(def example "[1,1]
              [2,2]
              [3,3]
              [4,4]")
;; => [[[[1,1],[2,2]],[3,3]],[4,4]]:

(def example2 "[1,1]
              [2,2]
              [3,3]
              [4,4]
              [5,5]")
; => [[[[3,0],[5,3]],[4,4]],[5,5]]

(def example3 "[1,1]
              [2,2]
              [3,3]
              [4,4]
              [5,5]
              [6,6] ")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map read-string))) 

(defn- number-to-the-left [loc]
  (loop [loc (zip/prev loc)]
    (cond
      (nil? loc) nil
      (number? (zip/node loc)) loc
      :else (recur (zip/prev loc)))))

(defn- number-to-the-right [loc]
  (loop [loc (zip/next loc)]
    (cond
      (nil? loc) nil
      (number? (zip/node loc)) loc
      :else (recur (zip/next loc)))))

(defn- explode? [loc]
  (and (>= (count (zip/path loc)) 4)
       (vector? (zip/node loc))
       (every? number? (zip/node loc))))

(defn- move-to-exploding-pair [loc]
  (loop [loc loc]
    (if (explode? loc)
      loc
      (recur (zip/next loc)))))

(defn- edit [loc getter value]
  (if-some [node (getter loc)]
    (zip/edit node #(+ % value))
    loc))

(defn- explode [loc]
  (let [[x y] (zip/node loc)]
    (-> loc
        (edit number-to-the-left x)
        move-to-exploding-pair
        (zip/replace 0)
        (edit number-to-the-right y))))

(defn reduce-pair [pair]
  (loop [loc (zip/vector-zip pair)]
    (if (zip/end? loc)
      (zip/root loc)
      (if (explode? loc)
        (let [exploded (explode loc)]
          (if (= (zip/root loc) (zip/root exploded))
            (zip/root loc)
            (recur (-> exploded zip/root zip/vector-zip))))
        (recur (zip/next loc))))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day17/input.txt")))
  ([input]
   (let [snail-list (parse input)]
     (reduce (fn [acc snail] (reduce-pair [acc snail]))
             snail-list))))

(comment
(parse example)
(part-one example)
(part-one example2)
(part-one example3)

(nth (iterate reduce-pair [[[[[9,8],1],2],3],4]) 1)
(nth (iterate reduce-pair [7,[6,[5,[4,[3,2]]]]]) 1)
(nth (iterate reduce-pair [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]]) 1)
(nth (iterate reduce-pair [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]]) 2))
