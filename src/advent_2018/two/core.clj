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

(defn- get-value-with-diff-one
  ([head values]
   
   (get-value-with-diff-one values head values 0))

  ([original-vals head values diff]
    (if-let [c (first head)]
      (if (= c (first values))
        (recur original-vals (rest head) (rest values) diff)
        (if (zero? diff)
          (recur original-vals (rest head) (rest values) (inc diff))
          nil))
        original-vals)))

(defn- get-char-to-remove-from-head [left right]
  (if-let [[head & tail] left]
    (if-not (= head (first right))
      head
      (recur tail (rest right)))))


(defn part-two [input-path]
  (let [values (->> (read-input input-path)
                  (map #(s/split % #"")))]
    
    (loop [values values]
      (if-let [[head & tail] values]
        (let [result (->> tail
                          (map (partial get-value-with-diff-one head))
                          (filter #(not (nil? %)))
                          (first))]

          (if-not (empty? result)
            (let [char-to-remove (get-char-to-remove-from-head head result)]
              (s/replace-first (s/join head) (re-pattern char-to-remove) ""))
            (recur tail)))))))

;; (part-two "./src/advent_2018/two/test-input2.txt")
(part-two "./src/advent_2018/two/input.txt")

