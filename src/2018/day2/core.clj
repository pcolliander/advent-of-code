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


(def example2
"
abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab")

(defn- get-freqs [lines]
  (->> lines
       (map frequencies)
       (map vals)
       ;; (map set)))
       (map distinct)
       flatten))

(defn part-one
  ([] (part-one (slurp "./src/2018/day2/input.txt")))
  ([input]
   (let [freqs (get-freqs (s/split-lines input))
         twos (count (filter #{2} freqs))
         threes (count (filter #{3} freqs))]
         ;; twos (count (filter #(contains? % 2) freqs))
         ;; threes (count (filter #(contains? % 3) freqs))]
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
  (part-one example2)
  (part-two example)
  (part-two))
