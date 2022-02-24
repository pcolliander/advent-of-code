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

(defn- explode [pair]
  (let [left (ffirst pair)
        right (-> pair first second)]

    [0 [(+ right (-> pair second first) ) (-> pair second second)]]))

(zip

(explode [[1,1],2])

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map read-string))) 

; iterate all the way down the nesting until it's one step above the 'bottom', where you'll have natural numbers.
; return the natural number and the nesting level.


; => [[[[3,0],[5,3]],[4,4]],[5,5]]

; try to use zippers to solve this issue. *(It seems to be the right data structure.)*

(defn- reduce-pair
  ([pair] (reduce-pair pair 1 nil))
  ([[left right] nested-level side]

     (println :root/left left)
     (println :root/right right)

     (let [left-pair (if (every? number? (vector left))
                         left
                       (reduce-pair left (inc nested-level) :left))
           right-pair (if (every? number? (vector right))
                        right
                        (reduce-pair right (inc nested-level) :right))

           ; try to apply fn-right / fn-left at every level (wherever there's a natural number to apply it to).
           [left-pair' right-pair' fn-left fn-right] (cond 
                                                       (and
                                                         (= nested-level 4)
                                                         (vector? left-pair)) (let [[lx ly] left-pair
                                                                                    [rx ry] right-pair]
                                                                                [0 [(+ ly rx) ry] (partial + lx) nil])
                                                       (and (= nested-level 4)
                                                            (vector? right-pair)) (let [[rx ry] right-pair]
                                                                                    [(+ left-pair rx)  0] nil (partial + ry))
                                                       :else 
                                                       [left-pair right-pair])]

       (println :left-pair left-pair)
       (println :right-pair right-pair)

       (->> [left-pair' right-pair' fn-left fn-right]
            (filterv some?)))))

(nth (iterate reduce-pair [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]]) 2)
(nth (iterate reduce-pair [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]]) 1)

;; (def data (zip/vector-zip [1 2 3 [1 2 [2 :error]]]))
;;
;;
;; (map zip/node (take 15 (->> (zip/vector-zip [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]])
;;                             (iterate zip/next))))
;;
;; (loop [location (zip/vector-zip [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]]) 
;;        depth 0]
;;   (println :location (zip/node location))
;;   (println :depth depth)
;;   (if (zip/end? location)
;;     (zip/node location)
;;     (recur
;;       (zip/next location)
;;       (if (zip/branch? location)
;;         (inc depth)
;;         depth))))
;;
;; (-> (zip/vector-zip [[[[[1,1],[2,2]],[3,3]],[4,4]] [5 5]])
;;     (zip/down)
;;     (zip/right)
;;     (zip/up)
;;     (zip/down)
;;     (zip/down)
;;     (zip/rightmost)
;;     ;; (zip/node)
;;     (zip/branch?)
;;     )
;;     #_(zip/down))

; => [[[[3,0],[5,3]],[4,4]],[5,5]]:


;;
;;    (let [child (reduce-pair left right nested)]
;;
;;      (cond
;;        (= nested 4)   ; explode
;;        (>= left 10)   ; split
;;        (>= right 10)  ; split
;;
;;        (cond
;;          (number (first pair))
;;
;;          (if (number? level)
;;            nested ; check if higher than 10
;;            (reduce-pair level (inc nested)))
;;          nested)))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day17/input.txt")))
  ([input]
   (let [snail-list (parse input)]
     (loop [snail-fish (drop 2 snail-list)
            sum (take 2 snail-list)]
       (if-let [snail (first snail-fish)]
         (let [pair (reduce-pair [sum snail])]
           (recur (next snail-fish) pair))
         sum)))))

(comment
(part-one example)
(part-one example2)
(part-one))
