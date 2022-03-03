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

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (mapv read-string))) 

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
  (println :explode (zip/node loc))
  (let [[x y] (zip/node loc)]
    (-> loc
        (edit number-to-the-left x)
        move-to-exploding-pair
        (zip/replace 0)
        (edit number-to-the-right y))))

(defn- split? [loc]
  (and
    (number? (zip/node loc))
    (> (zip/node loc) 10)))

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
(part-one example4))
