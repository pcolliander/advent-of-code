(ns ad-of-code.10.core
  (:require [clojure.string :as str]))

(defn- swap [items first-index last-index]
  (assoc items first-index (items last-index) last-index (items first-index))) 

(defn swap-items [items current-position length]
  (loop [no-of-swaps (unchecked-divide-int length 2)
         first-index current-position
         last-index (mod (- (+ length current-position) 1) (count items))  ; - 1 since it's array index (starts at 0)
         items items]

    (if (= no-of-swaps 0)
      items
      (recur
        (dec no-of-swaps)
        (mod (inc first-index) (count items))
        (mod (dec last-index) (count items))
        (swap items first-index last-index)))))


; Part 1
(defn f [input-path items]
  (let [input (slurp input-path)
        sequence-of-lengths-as-strings (-> input (str/trim) (str/split #","))
        sequence-of-lengths (map #(Integer/parseInt %) sequence-of-lengths-as-strings) ]

    (loop [items items
           current-position 0
           skip-size 0
           lengths sequence-of-lengths]

      (if-let [length (first lengths)]
        (recur 
          (swap-items items current-position length)
          (mod (+ current-position length skip-size) (count items))
          (inc skip-size)
          (rest lengths))

        (println "result is " (* (get items 0) (get items 1)))))))

;; (f "./src/ad_of_code/10/test-input.txt" [0,1,2,3,4])
(f "./src/ad_of_code/10/puzzle-input.txt" (into [] (range 256)))


