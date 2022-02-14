(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.string :as string]
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

(comment
(part-one example)
(part-one))
