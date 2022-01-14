(->> ns advent-of-code-2021.one.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]))

(defn- count-increases [xs]
  (->> (map - xs (next xs))
       (filter neg?)
       (count)))

(defn- read-input [input-path]
  (let [input (slurp (or input-path "test-input.txt"))]
    (->> (string/split-lines input)
         (map edn/read-string))))

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

(defn part-one [input-path]
   (count-increases (read-input input-path)))

(defn part-two [input-path]
  (let [lines (read-input input-path)
        xs (map + lines (next lines) (nnext lines))]
    (count-increases xs)))

(comment
  (f "input.txt")
  (part-one "input.txt")
  (part-two "input.txt"))

