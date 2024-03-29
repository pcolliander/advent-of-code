(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.zip :as zip]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :refer [priority-map]]))

(def example "[1,1]
             [2,2]
             [3,3]
             [4,4]")

(def example2 "[1,1]
              [2,2]
              [3,3]
              [4,4]
              [5,5]")

(def example3 "[1,1]
              [2,2]
              [3,3]
              [4,4]
              [5,5]
              [6,6] ")

(def example4 "[[[[4,3],4],4],[7,[[8,4],9]]] 
              [1,1]")


(def example5 "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
              [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
              [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
              [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
              [7,[5,[[3,8],[1,4]]]]
              [[2,[2,2]],[8,[8,1]]]
              [2,9]
              [1,[[[9,3],9],[[9,0],[0,7]]]]
              [[[5,[7,4]],7],1]
              [[[[4,2],2],6],[8,7]]")

(def homework "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
              [[[5,[2,8]],4],[5,[[9,9],0]]]
              [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
              [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
              [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
              [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
              [[[[5,4],[7,7]],8],[[8,3],8]]
              [[9,3],[[9,9],[6,[4,9]]]]
              [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
              [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (mapv read-string))) 

(defn- number-to-the-left [loc]
  (loop [loc (zip/prev loc)]
    (cond
      (nil? loc) nil
      (number? (zip/node loc)) loc
      (zip/end? loc) nil
      :else (recur (zip/prev loc)))))

(defn- number-to-the-right [loc]
  (loop [loc (zip/next loc)]
    (cond
      (nil? loc) nil
      (number? (zip/node loc)) loc
      (zip/end? loc) nil
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

(defn- split? [loc]
  (and
    (number? (zip/node loc))
    (>= (zip/node loc) 10)))

(defn- split-one [loc]
  (let [number (zip/node loc)]
    (-> loc
        (zip/replace [(-> number (/ 2) Math/floor int) (-> number (/ 2) Math/ceil int)])
        zip/root)))

(defn- split [snails]
  (loop [loc (zip/vector-zip snails)]
    (if (zip/end? loc)
      (zip/root loc)
      (if (split? loc)
        (split-one loc)
        (recur (zip/next loc))))))

(defn reduce-pair [pair]
  (loop [loc (zip/vector-zip pair)]
    (if (zip/end? loc)
      (let [result (split (zip/root loc))]
        (if (= result (zip/root loc))
          (zip/root loc)
          (reduce-pair result)))
      (if (explode? loc)
        (let [exploded (explode loc)]
          (recur (-> exploded zip/root zip/vector-zip)))
        (recur (zip/next loc))))))

(defn- magnitude [pair]
  (if (number? pair)
    pair
    (let [[a b] pair]
      (+ (* 3 (magnitude a)) (* 2 (magnitude b))))))


(defn part-one
  ([] (part-one (slurp "./src/2021/day18/input.txt")))
  ([input]
   (let [snail-list (parse input)]
     (->> snail-list
          (reduce (fn [acc snail]
                    (reduce-pair [acc snail])))
          magnitude))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day18/input.txt")))
  ([input]
   (let [snail-list (parse input)]
     (->> (for [a snail-list
                b snail-list
                :let [x (-> [a b] reduce-pair magnitude)
                      y (-> [b a] reduce-pair magnitude)]]
            (max x y))
          (reduce max)))))

(comment
  (parse example)
  (part-one example)
  (part-one example2)
  (part-one example3)
  (part-one example4)
  (part-one example5)

  (magnitude [9 1])
  (magnitude [[9,1],[1,9]])
  (magnitude [[1,2],[[3,4],5]])
  (magnitude [[[[0,7],4],[[7,8],[6,0]]],[8,1]])
  (magnitude [[[[1,1],[2,2]],[3,3]],[4,4]])
  (magnitude [[1,2],[[3,4],5]])
  (magnitude [[1,2],[[3,4],5]])

  (part-one homework)
  (part-one)

  (part-two homework)
  (part-two))


