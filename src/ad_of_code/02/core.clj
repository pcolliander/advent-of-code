(ns ad-of-code.02.core
  (:require [clojure.string :as str]))

; Part 1
(defn- calc-difference [row]
  (- (reduce max row ) (reduce min row )))

(defn checksum-calc [input-path]
  (let [file (slurp input-path)]
    (->> (str/split-lines file)
       (map #(str/split % #" ")) 
       (map (fn [row] (map #(Integer/parseInt %) row)))
       (map calc-difference)
       (reduce +))))

(checksum-calc "./src/ad_of_code/02/test-input.txt")
(checksum-calc "./src/ad_of_code/02/puzzle-input.txt")
    
; PART 2
(defn- find-and-divide-even-divisible-numbers [row]
  (let [length (count row)
        partitions (take length (partition length 1 (cycle row)))]
    
    (for [part partitions]
      (for [numb (rest part)]
        (if (= (mod (first part) numb) 0)
            (/ (first part) numb))))))

(defn sum-of-each-row [input-path]
  (let [file (slurp input-path)
        xf (comp 
             (map find-and-divide-even-divisible-numbers)
             (mapcat flatten)
             (remove nil?))]

    (->> (str/split-lines file)
        (map #(str/split % #" "))
        (map (fn [row] (map #(Integer/parseInt %) row)))
        (transduce xf +))))

(sum-of-each-row "./src/ad_of_code/02/test-input2.txt")
(sum-of-each-row "./src/ad_of_code/02/puzzle-input.txt")

