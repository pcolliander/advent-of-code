(ns ad-of-code.04.core
  (:require [clojure.string :as str]))

; Part 1
(defn calc-correct-passwords [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)]

  (loop [counter 0
         input-lines input-lines]

    (if-let [line (first input-lines)]
      (let [words (str/split line #" ")]
        (recur
          (if (apply distinct? words) (inc counter) counter)
          (rest input-lines)))
      counter))))

(calc-correct-passwords "./src/ad_of_code/04/puzzle-input.txt")

; Part 2
(defn- sort-words [input]
  (->> (str/split input #" ")
   (map sort)
   (map str/join)))

(defn calc-correct-passwords-anagram [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)]

  (loop [counter 0
         input-lines input-lines]

    (if-let [line (first input-lines)]
      (let [sorted-words (sort-words line)]
        (recur
          (if (apply distinct? sorted-words) (inc counter) counter)
          (rest input-lines)))
      counter))))

(calc-correct-passwords-anagram "./src/ad_of_code/04/puzzle-input.txt")

