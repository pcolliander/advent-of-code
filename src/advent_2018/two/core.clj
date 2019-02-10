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

(defn- get-value-with-diff-one [head right]
  (let [diff-by-one? (= 1 (count (filter false? (map = head right))))]
    (if diff-by-one?
      right)))

(defn- get-char-to-remove-from-head [left right]
  (let [comparisons (map (fn [l r]
                           (if (not= l r) (str l)))
                         left right)]

  (->> comparisons
    (some #(when (not (nil? %)) %)))

(defn part-two [input-path]
  (loop [values (read-input input-path)]
    (if-let [[head & tail] values]
      (let [result (->> tail
                    (map (partial get-value-with-diff-one head))
                    (some #(when (not (nil? %)) %)))]

        (if-not (nil? result)
          (let [char-to-remove (get-char-to-remove-from-head head result)]
            (s/replace-first head (re-pattern char-to-remove) ""))
          (recur tail))))))

(part-two "./src/advent_2018/two/test-input2.txt")
(part-two "./src/advent_2018/two/input.txt")

