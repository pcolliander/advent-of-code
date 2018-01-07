(ns ad-of-code.01.core
  (:require [clojure.string :as str]))

(defn- input-to-sequence-of-digits [input-string]
  (let [sequence-of-strings (-> input-string (str/trim) (str/split #""))]
     (->> sequence-of-strings
       (map #(Integer/parseInt %)))))

; Part 1
(defn get-sum-part-1 [input-path]
  (let [input-string (slurp input-path)
        sequence-of-digits (input-to-sequence-of-digits input-string)
        first-value (first sequence-of-digits)
        last-value (last sequence-of-digits)
        initial-sum (if (= first-value last-value) last-value 0)]

    (reduce
      (fn [acc next-value]
        (if (= (:prev-value acc) next-value)
          (merge acc {:sum (+ (:sum acc) next-value) :prev-value next-value})
          (merge acc {:prev-value next-value}) ))
      {:sum initial-sum :first-value first-value :prev-value nil}
      sequence-of-digits)))

(get-sum-part-1 "./src/ad_of_code/01/puzzle-input.txt" )

(defn get-sum-part-2 [input-path]
  (let [input-string (slurp input-path)
        sequence-of-digits (input-to-sequence-of-digits input-string)
        length-of-list (count sequence-of-digits)
        steps-to-take (/ length-of-list 2)
        inital-value {:current-item 0, :total-sum 0, :input-sequence sequence-of-digits}]

        (reduce
          (fn [acc next-value]
            (let [halfway-around-index (mod (+ (:current-item acc) steps-to-take) length-of-list)
                  value-halfway-around (nth (:input-sequence acc)  halfway-around-index)]

              (if (= next-value value-halfway-around)
                 (merge acc {:current-item (+ (:current-item acc) 1) :total-sum (+ (:total-sum acc) next-value)})
                 (merge acc {:current-item (+ (:current-item acc) 1)} ))))
          inital-value
          sequence-of-digits)))

(get-sum-part-2 "./src/ad_of_code/01/puzzle-input.txt")

