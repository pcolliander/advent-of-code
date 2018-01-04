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

    (loop [complete-register {} index 0 current-highest-value 0]
      (if (== index (count instruction-lines))
        (do
          (println "(part 1) result: " (reduce max (vals complete-register)))
          (println "(part 2) highest-value: " current-highest-value))
        (let [string (get instruction-lines index)
              instruction (str/split string  #" ")
              register (nth instruction 0)
              operator (nth instruction 1)
              amount (Integer/parseInt (nth instruction 2))
              register-in-condition (nth instruction 4)
              condition (nth instruction 5)
              second-operand (Integer/parseInt (nth instruction 6))]

            (if ((map-condition condition) (or (get complete-register register-in-condition) 0) second-operand)
              (let [updated-register (update-register operator amount register complete-register)
                    new-value (get updated-register register) 
                    highest-value (if (> new-value current-highest-value) new-value current-highest-value)]

                (recur (update-register operator amount register complete-register) (inc index) highest-value))
              (recur complete-register (inc index) current-highest-value)))))))

(f "./src/ad_of_code/08/puzzle-input.txt")
(f "./src/ad_of_code/08/test-input.txt")

