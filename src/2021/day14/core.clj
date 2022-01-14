(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.set :as cset]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "NNCB

             CH -> B
             HH -> N
             CB -> H
             NH -> C
             HB -> C
             HC -> B
             HN -> C
             NN -> C
             BH -> H
             NC -> B
             NB -> B
             BN -> B
             BB -> N
             BC -> B
             CC -> N
             CN -> C")

(defn- parse [input]
  (let [lines (string/split-lines input)
        template (first lines)
        insertion-rules (->> (drop 2 lines) (map string/trim) (map #(string/split % #" -> ")) (into {}))]
    {:template template
    :insertion-rules insertion-rules}))

(defn- compute-pairs [insertion-rules template]
  (let [pairs (map (fn [firs seco]
                     (let [pair (str firs seco)
                           rule-match (get insertion-rules pair)]
                       (str firs rule-match seco))) template (next template))]
    (apply str (first pairs) (->> (next pairs)
                                  (map #(subs % 1))))))

(defn- highest-minus-lowest-common [polymer] 
  (let [sorted-polymer-values (sort-by val polymer)]
    (- (-> sorted-polymer-values last second) (-> sorted-polymer-values first second))))


(defn part-one
  ([] (part-one (slurp "./src/2021/day14/input.txt")))
  ([input]
   (let [{:keys [template insertion-rules]} (parse input)
         polymer (nth (iterate (partial compute-pairs insertion-rules) template) 10)]

     (highest-minus-lowest-common (frequencies polymer)))))

(defn- step' [insertion-rules frequencies-polymer-template]
   (reduce-kv
     (fn [m [el1 el2 el3] v]
       (let [first-match (get insertion-rules (str el1 el2))
             second-match (get insertion-rules (str el2 el3))
             new-pair (str el1 first-match el2)
             new-second-pair (str el2 second-match el3)]

         (-> m
             (update new-pair (fnil + 0) v)
             (update new-second-pair (fnil + 0) v))))
     {}
     frequencies-polymer-template))

(defn part-two
   ([] (part-two (slurp "./src/2021/day14/input.txt")))
  ([input]
   (let [{:keys [template insertion-rules]} (parse input)
         last-letter (last template)
         initial-polymer  (map (fn [firs seco]
                         (let [pair (str firs seco)
                               rule-match (get insertion-rules pair)]
                           (str firs rule-match seco))) template (next template))

         polymer (nth (iterate (partial step' insertion-rules) (frequencies initial-polymer)) 39)

         after-40 (as-> polymer polymer$
                       (reduce-kv 
                         (fn [m [el1 el2 el3] v]
                           (-> m
                               (update el1 (fnil + 0) v)
                               (update el2 (fnil + 0) v)))
                         {}
                         polymer$)
                       (update polymer$ last-letter inc))]

     (highest-minus-lowest-common after-40))))

(comment
(parse example)

(part-one example)
(part-one)
(part-two example)
(part-two))
