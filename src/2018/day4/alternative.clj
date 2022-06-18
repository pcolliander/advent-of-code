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

(defn- parse [line]
  (let [[_ minute op] (re-find #"\[\d+-\d+-\d+ \d+:(\d+)\] (.*$)" line)]
    (case op
      "falls asleep" [:asleep (parse-long minute)]
      "wakes up"     [:awake  (parse-long minute)]
      (let [[_ id] (re-find #"#(\d+)" op)]
       [:guard (parse-long id)]))))

(let [lines (map parse (sort (s/split-lines (slurp "./src/2018/day4/input.txt") #_example) ))
      intervals (loop [guard nil
                        lines lines
                        result nil
                        asleep nil]
                   (if (empty? lines)
                     result
                     (let [[op arg] (first lines)]
                      (case op
                         :guard  (recur arg (next lines) result nil)
                         :asleep (recur guard (next lines) result arg)
                         :awake  (recur guard (next lines) (conj result [guard asleep arg]) nil)))))
      
      [id _] (->> intervals
                                 (reduce (fn [acc [guard asleep awake]]
                                           (update acc guard #(+ (or % 0) (- awake asleep))))

                                         {})
                                 (sort-by second)
                                 last)

      [minute _] (->> intervals
                                  (filter #(= id (first %)))
                                  (map (fn [[_ sleep-at wake-up]]
                                         (into {}
                                               (for [minute (range sleep-at wake-up)]
                                                 [minute 1] ))))
                                  (apply merge-with +)
                                  (sort-by second)
                                  last)]
  (* minute id))

