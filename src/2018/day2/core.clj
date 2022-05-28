(ns advent_2018.two.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example
"abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz")

(defn part-one
  ([] (part-one (slurp "./src/2018/day2/input.txt")))
  ([input]
   (let [{:keys [twos threes]} (->> (s/split-lines (slurp "./src/2018/day2/input.txt"))
                                    (map frequencies)
                                    (map vals)
                                    (map frequencies)
                                    (reduce (fn [sum {twos 2 threes 3}]
                                              (cond-> sum
                                                twos (update :twos inc)
                                                threes (update :threes inc)))
                                            {:twos 0 :threes 0}))]
        (* twos threes))))

(defn- get-differing-chars [left right]
  (->> (map vector left right)
    (filter (partial apply not=))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day2/input.txt")))
  ([input]
  (loop [values (s/split-lines input)]
    (if-let [[head & tail] values]
      (let [result (->> tail
                     (some (fn [word]
                               (let [differing-chars (get-differing-chars head word)]
                                 (when (= 1 (count differing-chars))
                                   differing-chars)))))]
        (if result
          (s/replace-first head (re-pattern (-> result ffirst str)) "")
          (recur tail)))))))

(comment
  (part-one)
  (part-two example)
  (part-two))
