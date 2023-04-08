(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(defonce p (p/open))
(add-tap #'p/submit)

(def example
"noop
addx 3
addx -5")

(defn parse [input] 
  (flatten (map  (fn [x]
          (let [[_ m] (re-find #"noop|addx (-*\d+)" x)]
            (if m
              [:noop (parse-long m)]
              :noop)))
  (string/split-lines input))))

(def cycles [20 60 100 140 180 220])

(defn part-one
  ([] (part-one (slurp "./src/2022/day10/input.txt")))
  ([input]
   (loop [cycle-n 1
          x 1
          instructions (parse input)
          signal-strength 0]
     (if-let [ex (first instructions)]
       (recur (inc cycle-n)
              (if (= :noop ex) x (+ x ex))
              (next instructions)
              (if (some #{cycle-n} cycles)
                (+ signal-strength (* cycle-n x))
                signal-strength))
       signal-strength))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day10/input.txt")))
  ([input]
   (let [pixels (loop [crt 0
                       x 1
                       instructions (parse input)
                       pixels []]
                  (if-let [ex (first instructions)]
                    (let [visible? (some #{(mod crt 40)} [(dec x) x (inc x)]) ]
                      (recur (inc crt)
                             (if (= :noop ex) x (+ x ex))
                             (next instructions)
                             (conj pixels (if visible? "█" "░"))))
                    pixels))
         drawing (->> pixels (partition 40) (mapv #(mapv identity %))) ]

     (dotimes [high 6]
       (dotimes [wide 40]
         (print (get-in drawing [high wide])))
       (println)))))

(comment
  (part-one example)
  (part-one (slurp "./src/2022/day10/large-example.txt"))
  (part-one)
  (part-two example)
  (part-two (slurp "./src/2022/day10/large-example.txt"))
  (part-two))

