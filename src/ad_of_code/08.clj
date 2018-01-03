(ns ad-of-code.08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

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
      (+ (or (get complete-register register) 0) amount )
      (- (or (get complete-register register) 0) amount ))))

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

          (if-let [register-value (get complete-register register-in-condition)]
            (if ((map-condition condition) register-value second-operand)
              (recur (update-register operator amount register complete-register) (inc index))
              (recur complete-register (inc index)))

            (if ((map-condition condition) 0 second-operand)
              (recur (update-register operator amount register complete-register) (inc index))
              (recur complete-register (inc index)))))))))

(f "./src/ad_of_code/puzzle-input.txt")
(f "./src/ad_of_code/test-input.txt")

