(ns advent_2018.two.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(defn- read-input [input-path]
  (->> (s/split-lines (slurp input-path))))

(defn part-one [input-path]
  (let [{:keys [twos threes]} (->> (read-input input-path)
                               (map #(frequencies (s/split % #"")))
                               (map vals)
                               (reduce (fn [prev n]
                                         (let [s (set n)]
                                           (-> prev
                                               (update :twos (if (contains? s 2) (fnil inc 0) identity))
                                               (update :threes (if (contains? s 3) (fnil inc 0) identity))))) {}))] 
  (* twos threes)))

;; (part-one "./src/advent_2018/two/test-input.txt")
(part-one "./src/advent_2018/two/input.txt")

(defn- get-differing-chars [left right]
  (->> (map vector left right)
    (filter (partial apply not=))))

(defn part-two [input-path]
  (loop [values (read-input input-path)]
    (if-let [[head & tail] values]
      (let [result (->> tail
                     (reduce (fn [prev n]
                               (let [differing-chars (get-differing-chars head n)]
                                 (if (= 1 (count differing-chars))
                                   {:differing-char (ffirst differing-chars)}
                                   prev)))))]

        (if-not (nil? (:differing-char result))
          (s/replace-first head (re-pattern (str (:differing-char result))) "")
          (recur tail))))))

(part-two "./src/advent_2018/two/test-input2.txt")
(part-two "./src/advent_2018/two/input.txt")

; mxhwoglxgeauywfkztndcvjqr

