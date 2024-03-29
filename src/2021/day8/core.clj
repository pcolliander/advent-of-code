(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as cset]
            [clojure.pprint :as pp]))

(def larger-example "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
              edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
              fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
              fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
              aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
              fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
              dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
              bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
              egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
              gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def example "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map #(string/split % #" \| "))
       (map (fn [[signal-patterns output-values]]
              [(->> (string/split signal-patterns #" ") (map string/trim))
               (->> (string/split output-values #" ") (map string/trim))]))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day8/input.txt")))
  ([input]
   (let [values (parse input)]
     (->> values
          (mapcat (fn [[_ output-values]]
                    (map count output-values)))
          (filter #(#{2 4 3 7} %))
          count))))

(defn- get-six [patterns one]
  (let [[top-right segment-six] (->> patterns
                                     (some (fn [pattern]
                                             (let [difference (cset/difference (set one) (set pattern))]
                                               (when (and 
                                                       (= 1 (count difference))
                                                       (= 6 (count pattern)))
                                                 [(first difference) pattern])))))
        bottom-right (cset/difference (set one) #{top-right})]

    [{:bottom-right bottom-right}
     segment-six]))

(defn- get-five [segment-six patterns]
  (let [[value segment-five] (->> patterns
                                  (some (fn [pattern]
                                          (let [difference (cset/difference (set segment-six) (set pattern))]
                                            (when (and 
                                                    (= 1 (count difference))
                                                    (= 5 (count pattern)))
                                              [(first difference) pattern])))))]
    [{:bottom-left value}
      segment-five]))

(defn- get-zero [patterns six nine]
  (some (fn [pattern]
          (when (= 6 (count pattern))
            pattern))
        (filter (complement #{six nine}) patterns)))

(defn- get-nine [patterns eight bottom-left]
  (some (fn [pattern]
          (when
            (and
              (= 6 (count pattern))
              (= 
                (set eight)
                (cset/union (set pattern) #{bottom-left})))
            pattern))
        patterns))

(defn- get-two [patterns bottom-right]
 (some (fn [pattern]
          (when
            (and
              (= 5 (count pattern))
              (not (contains? (set pattern) (first bottom-right))))
            pattern))
        patterns))

(defn- build-wire-connections [[patterns output-value]]
  (let [one (some #(when (= 2 (count %)) %) patterns)
        four (some #(when (= 4 (count %)) %) patterns) 
        seven (some #(when (= 3 (count %)) %) patterns) 
        eight (some #(when (= 7 (count %)) %) patterns)
        [{:keys [bottom-right]} six] (get-six patterns one)
        [{:keys [bottom-left]} five] (get-five six patterns)
        nine (get-nine patterns eight bottom-left)
        zero (get-zero patterns six nine)
        two (get-two patterns bottom-right)
        three (first (filter (complement #{zero one two four five six seven eight nine}) patterns))
        all (into {} (map-indexed (fn [index value]
                                    [(set value) index])
                                   [zero one two three four five six seven eight nine]))]

    (->> output-value
         (map set)
         (map #(get all %))
         string/join
         Integer/parseInt)))

(defn part-two
  ([] (part-two (slurp "./src/2021/day8/input.txt")))
  ([input]
   (let [values (parse input)]
     (->> values
          (map build-wire-connections)
          (reduce +)))))

(comment
(part-one larger-example)
(part-one)

(part-two example)
(part-two larger-example)
(part-two))
