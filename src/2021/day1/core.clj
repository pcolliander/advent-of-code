(->> ns advent-of-code-2021.one.core
     (:require [clojure.edn :as edn]
               [clojure.string :as string]))
(def example "199
             200
             208
             210
             200
             207
             240
             269
             260
             263")

(defn- count-increases [xs]
  (->> (map - xs (next xs))
       (filter neg?)
       (count)))

(defn- parse [input]
  (->> (string/split-lines input)
       (map edn/read-string)))

; naive solution.
(defn f [input-path]
 (let [lines (read-input input-path)
       result (->> lines
                   (reduce (fn [result n]
                             (if (and (:prev result) (> n (:prev result)))
                               {:total (inc (:total result))
                                :prev n}
                               (assoc result :prev n)))
                           {:total 0 :prev nil}))]
   (:total result)))

(defn part-one
  ([]  (part-one (slurp "./src/2021/day1/input.txt")))
  ([input]
   (count-increases (parse input))))

(defn part-two
  ([]  (part-two (slurp "./src/2021/day1/input.txt")))
  ([input]
  (let [lines (parse input)
        xs (map + lines (next lines) (nnext lines))]
    (count-increases xs))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))

