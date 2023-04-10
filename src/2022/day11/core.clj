(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]
             [portal.api :as p]))

(def p (p/open))
(add-tap #'p/submit)

(def example
"Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn- parse [input]
  (->> (string/split input #"\n\n")
       (map (comp #(map string/trim %) string/split-lines))
       (map parse-instruction)
       (into {})))

(defn- parse-instruction [[n items operation condition if-true if-false]]
  (let [[_ monkey-n] (re-find #"Monkey (\d)" n)
        starting-items (filter (complement string/blank?) (map second (re-seq #" ?(\d*),?" items)))
        [_ operation] (re-find #"new = (.*)" operation)
        [_ condition] (re-find #"Test: divisible by (\d*)" condition)
        [_ if-true] (re-find #"If true: throw to monkey (\d*)" if-true)
        [_ if-false] (re-find #"If false: throw to monkey (\d*)" if-false)]

    [monkey-n
     {:items starting-items
      :operation operation
      :condition condition
      :if-true if-true
      :if-false if-false}]))

(defn- apply-operation [operation value]
  (let [[operand operator operand-2]  (-> operation
                                        (string/replace #"old" (str value))
                                        (string/split #" "))]
    (case operator
      "*" (* (parse-long operand) (parse-long operand-2))
      "+" (+ (parse-long operand) (parse-long operand-2)))))

(defn- round [turns]
  (->> (keys turns)
       (reduce 
         (fn [state n]
           (let [{:keys [items operation if-true if-false condition inspected]} (get state n)
                 items (map (comp #(quot % 3) (partial apply-operation operation)) items)
                 inspected-count ((fnil + 0) inspected (count items))]
                            
             (->> items
                  (reduce (fn [state' item]
                            (cond-> state'
                              true (assoc-in [n :items] [])
                              true (assoc-in [n :inspected] inspected-count)
                              (zero? (mod item (parse-long condition))) (update-in [if-true :items] #(conj % item))
                              (pos?  (mod item (parse-long condition))) (update-in [if-false :items] #(conj % item))))
                          state))))
         turns)))


(defn part-one
  ([] (part-one (slurp "./src/2022/day11/input.txt")))
  ([input]
   (let [state (parse input)
         state' (loop [n 20
                       state state]
                  (if (pos? n)
                    (recur (dec n) (round state))
                    state))]
     (apply * (take 2 (sort > (map :inspected (vals state'))))))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day11/input.txt")))
  ([input]
   ()))

(parse example)

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))

