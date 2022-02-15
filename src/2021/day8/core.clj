(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as cset]
            [clojure.pprint :as pp]))

(def example "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
              edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
              fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
              fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
              aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
              fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
              dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
              bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
              egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
              gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def example2 "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map #(string/split % #" \| "))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day8/input.txt")))
  ([input]
   (let [values (parse input)]
     (->> values
          (map (fn [[_ output-values]]
                    (->> (string/split output-values #" ")
                         (map count)
                         (filter (fn [output-value]
                                   (some #{output-value} #{2 4 3 7}))))))
          (map #(reduce (fn [acc n]
                          (inc acc)) 0 %))
          (reduce +)))))

(def digit
  {:top nil
   :top-left nil
   :top-right nil
   :middle nil
   :bottom-left nil
   :bottom-right nil
   :bottom nil})

(defn- one? [pattern]
  (= 2 (count pattern)))

(defn- seven? [pattern]
  (= 3 (count pattern)))

(defn- top [digit patterns]
  (let [[one seven] (concat
                      (filter one? patterns)
                      (filter seven? patterns))]
  (assoc digit :top (first (cset/difference (set seven) (set one))))))

(defn- top-right [digit patterns]
  (let [one (->> patterns (filter one?) first)
        value (some (fn [pattern]
                      (let [difference (cset/difference (set one) (set pattern))]
                        (when (= 1 (count difference))
                          (first difference)))) patterns)]
    (assoc digit :top-right value)))

(defn- build-wire-connections [signal-patterns]
  (-> digit
      (top signal-patterns)
      (top-right signal-patterns)))

(build-wire-connections ["acedgfb" "dab" "eafb" "ab" "cefabd" "cdfgeb"])

;; figure out **top** by comparing 7 with 1.
;; figure out top-right by diffing 6 and 1
;; figure out bottom-left by comparing 5 and 6 (six found in previous step).

;; figure out top-right by finding 5 & 6 that's both missing the number in top-right or bottom-right.
;; then compare 5 & 6 to see which is missing in bottom-left.
;; the search for 0 which will be missing the middle part.


(defn part-two
  ([] (part-two (slurp "./src/2021/day8/input.txt")))
  ([input]
   (let [values (parse input)]
     values
     (->> values
          (map first)
          (map #(string/split % #" "))
          (map build-wire-connections)))))
(comment
(part-one example)
(part-one)
(part-two example))
