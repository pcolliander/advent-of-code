(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "3,4,3,1,2")

(defn- parse [input-path]
   (let [fish (->> (string/split (slurp input-path) #",")
                   (map string/trim)
                   (map #(Integer/parseInt %)))]

     fish))

;; (defn- progress [fish]
;;   (concat
;;     (map #(if (zero? %) 6 (dec %)) fish)
;;     (repeat (count (filter zero? fish)) 8)))

;; (nth (iterate progress (parse "./six/test-input.txt")) 18)

(defn progress [lanterns]
  (->> lanterns
       (flatten)
       (map dec)
       (map (fn [lantern]
              (if (neg? lantern)
                [6 8]
                lantern)))))

(defn part-one
  ([] (part-one "./six/test-input.txt"))
  ([input-path]
   (let [lanterns (parse input-path)]

     (loop [day 0 lanterns lanterns]
       (if (<= day 80)
         (recur (inc day)
                (progress lanterns))
         (count lanterns))))))

(defn progress' [input]
  (reduce-kv (fn [acc k v]
               (if (zero? k)
                 (-> acc
                     (assoc 8 v)
                     (update 6 (fnil + 0) v))
                 (-> acc
                     (update (dec k) (fnil + 0) v))))

           {}
           input))

(defn part-two
  ([] (part-two "./six/test-input.txt"))
  ([input-path]
   (let [lanterns (parse input-path)]
     (loop [day 0 lanterns (frequencies lanterns)]
       (println :day day)
       (if (>= day 256)
         (reduce + (vals lanterns))
         (recur (inc day) (progress' lanterns)))))))

(comment
(part-one  #_"./six/input.txt")
(part-two  #_"./six/input.txt"))
