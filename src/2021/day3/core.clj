(ns advent-of-code-2021.two.core
  (:require [clojure.string :as string]))

(def example "00100
             11110
             10110
             10111
             10101
             01111
             00111
             11100
             10000
             11001
             00010
             01010")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)))

(defn- bit->decimal [bit]
  (Integer/parseInt bit 2))

(defn- gamma->epsilon [gamma]
  (string/join
    (->> (vec gamma)
         (map (fn [bit]
               (if (= bit \0)
                 "1"
                 "0"))))))

(defn- remove-first-digit [line]
  (string/join (next (vec line))))

(defn part-one
  ([]  (part-one (slurp "./src/2021/day3/input.txt")))
  ([input]
   (let [lines (parse input)]
     (loop [lines lines
            gamma nil]
       (if (ffirst lines)
         (let [digit (->> lines 
                          (map first)
                          (frequencies)
                          (sort-by val)
                          (reverse)
                          (ffirst))]
           (recur (map remove-first-digit lines) (str gamma digit)))
         (let [epsilon (gamma->epsilon gamma)]
           (* (bit->decimal gamma) (bit->decimal epsilon))))))))

;; --------- PART 2 -----------

(defn- with-digit [digit letter-n line]
  (string/starts-with? (subs line letter-n) (str digit)))

(defn- x [line]
  [((complement pos?) (-> line key str Integer/parseInt)) (val line)])

(defn- find-og-rating [lines]
 (loop [lines lines letter-n 0]
   (if (or (= 0 (count lines)) (= 1 (count lines)))
     (first lines)
     (let [digit (->> lines 
                   (map (fn [line]
                          (nthnext line letter-n)))
                   (map first)
                   (frequencies)
                   (sort-by x)
                   (reverse)
                   (ffirst))]

       (recur (filter (partial with-digit digit letter-n) lines) (inc letter-n))))))

(defn- y [line]
  [((complement neg?) (-> line key str Integer/parseInt)) (val line)])

(defn- find-co2-rating [lines]
 (loop [lines lines letter-n 0]
   (if (or (= 0 (count lines)) (= 1 (count lines)))
     (first lines)
     (let [digit (->> lines 
                   (map (fn [line]
                          (nthnext line letter-n)))
                   (map first)
                   (frequencies)
                   (sort-by y)
                   (ffirst))]

       (recur (filter (partial with-digit digit letter-n) lines) (inc letter-n)))))) 

(defn part-two
  ([]  (part-two (slurp "./src/2021/day3/input.txt")))
  ([input]
   (let [lines (parse input)
         og-rating (find-og-rating lines)
         co2-rating (find-co2-rating lines)]
    (* (bit->decimal og-rating) (bit->decimal co2-rating)))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
