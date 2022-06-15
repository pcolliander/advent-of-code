(ns advent_2018.four.core
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def example
"[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
")

(defn- get-minute-asleep-most [actions]
  (->> actions
       (partition 2)
       (mapcat (fn [[slept-at wake-up-at]]
                 (range (:minute slept-at) (:minute wake-up-at))))
       frequencies
       (sort-by val)
       last))

(defn- get-total-minutes-asleep [[guard actions]]
  [guard (->> actions
              (partition 2)
              (reduce (fn [acc [slept-at wake-up-at]]
                        (+ acc (- (:minute wake-up-at) (:minute slept-at))))
                      0))])

(defn- parse [line]
  (let [[_ minute op] (re-find #"\[\d+-\d+-\d+ \d+:(\d+)\] (.*$)" line)]
    {:minute (parse-long minute)
     :op op}))

(defn- get-actions-by-guard [lines]
  (loop [lines lines
         actions-by-guard {}]
    (if-let [line (first lines)]
      (let [[_ guard] (re-find #"#(\d+)" (:op line))
            actions (->> (next lines) (take-while #(not (re-find #"#(\d+)" (:op %)))))]
        (recur (drop-while #(not (re-find #"#(\d+)" (:op %))) (next lines))
               (update actions-by-guard guard concat actions)))
      actions-by-guard)))

(defn part-one
  ([] (part-one (slurp "./src/2018/day4/input.txt")))
  ([input]
   (let [lines (map parse (sort (s/split-lines input)))
         actions-by-guard (get-actions-by-guard lines)
         [guard-id _] (->> actions-by-guard
                           (map get-total-minutes-asleep)
                           (sort-by second)
                           last)
         most-minute-asleep (-> (get-minute-asleep-most (get actions-by-guard guard-id)) first)]
     (* most-minute-asleep (parse-long guard-id)))))

(defn part-two
  ([] (part-two (slurp "./src/2018/day4/input.txt")))
  ([input]
   (let [lines (map parse (sort (s/split-lines input)))
         actions-by-guard (get-actions-by-guard lines)
         [guard-id [minute _]] (->> (for [id (keys actions-by-guard)
                                          :let [most-asleep (get-minute-asleep-most (get actions-by-guard id))]]
                                      [id most-asleep])
                                    (sort-by #(-> % second second))
                                    last)]
     (* (parse-long guard-id) minute))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))
