(ns ad-of-code.08.core
  (:require [clojure.string :as str]))

(defn- map-condition [condition]
  (case condition
    ">" >
    "<" <
    ">=" >=
    "==" ==
    "!=" not=
    "<=" <=))

(defn- update-register [op amount register complete-register]
  (assoc complete-register register 
    (if (= op "inc")
      (+ (or (get complete-register register) 0) amount)
      (- (or (get complete-register register) 0) amount))))

(defn f [input-path]
  (let [file (slurp input-path)
        instruction-lines (str/split-lines file)]

    (loop [complete-register {} index 0]
      (if (== index (count instruction-lines))
        (println (reduce max (vals complete-register)))
        (let [string (get instruction-lines index)
              instruction (str/split string  #" ")
              register (nth instruction 0)
              operator (nth instruction 1)
              amount (Integer/parseInt (nth instruction 2))
              register-in-condition (nth instruction 4)
              condition (nth instruction 5)
              second-operand (Integer/parseInt (nth instruction 6))]

            (if ((map-condition condition) (or (get complete-register register-in-condition) 0) second-operand)
              (recur (update-register operator amount register complete-register) (inc index))
              (recur complete-register (inc index))) )))))

(f "./src/ad_of_code/08/puzzle-input.txt")
(f "./src/ad_of_code/08/test-input.txt")



;; (let [updated-register (update-register operator amount register complete-register)
;;       new-value (get update-register register)
;;       new-highest-value (if (< value highest-value) new-value highest-value)]




