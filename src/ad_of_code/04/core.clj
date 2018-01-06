(ns ad-of-code.04.core
  (:require [clojure.string :as str]))

; Part 1
(defn calc-correct-passwords [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)]

  (loop [counter 0 n (count input-lines)]
    (if (= n 0)
      counter
      (let [words (str/split (input-lines (dec n)) #" ")]
        (recur
          (if (apply distinct? words) (inc counter) counter)
          (dec n)))))))

(calc-correct-passwords "./src/ad_of_code/04/puzzle-input.txt")

; Part 2
(defn- sort-words [input]
  (->> (str/split input #" ")
   (map sort)
   (map str/join)))

(defn calc-correct-passwords-anagram [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)]

  (loop [counter 0 n (count input-lines)]
    (if (= n 0)
      counter
      (let [sorted-words (-> (input-lines (dec n)) sort-words)]
        (recur
          (if (apply distinct? sorted-words) (inc counter) counter)
          (dec n)))))))

(calc-correct-passwords-anagram "./src/ad_of_code/04/puzzle-input.txt")

