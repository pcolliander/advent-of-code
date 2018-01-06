(ns ad-of-code.05.core
  (:require [clojure.string :as str]))

; Part 1
(defn calc-steps-to-exit [input-path] 
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        rows (map #(Integer/parseInt %) input-lines) ]
  
    (loop [steps 1 current-index 0 array (vec rows)]
      (let [steps-to-jump (get array current-index)
            new-index (+ current-index steps-to-jump)]

        (if (get array new-index)
          (recur (inc steps) new-index (update array current-index inc))
          steps)))))

(calc-steps-to-exit "./src/ad_of_code/05/test-input.txt")
(calc-steps-to-exit "./src/ad_of_code/05/puzzle-input.txt")

; Part 2
(defn calc-steps-to-exit-part-2 [input-path] 
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        rows (map #(Integer/parseInt %) input-lines) ]

    (loop [steps 1 current-index 0 array (vec rows)]
      (let [steps-to-jump (get array current-index)
            next-index (+ current-index steps-to-jump)]

        (if (get array next-index)
          (recur
            (inc steps)
            next-index
            (if (>= steps-to-jump 3)
              (update array current-index dec)
              (update array current-index inc)))
          steps)))))

(calc-steps-to-exit-part-2 "./src/ad_of_code/05/test-input.txt")
(calc-steps-to-exit-part-2 "./src/ad_of_code/05/puzzle-input.txt")
