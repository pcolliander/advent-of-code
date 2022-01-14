(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.set :as cset]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "start-A
              start-b
              A-c
              A-b
              b-d
              A-end
              b-end")

(def larger-example "dc-end
                    HN-start
                    start-kj
                    dc-start
                    dc-HN
                    LN-dc
                    HN-end
                    kj-sa
                    kj-HN
                    kj-dc")

(def even-larger-example "
  fs-end
                         he-DX
                         fs-he
                         start-DX
                         pj-DX
                         end-zg
                         zg-sl
                         zg-pj
                         pj-he
                         RW-he
                         fs-DX
                         pj-RW
                         zg-RW
                         start-pj
                         he-WI
                         zg-he
                         pj-fs
                         start-RW")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)
       (map #(string/split % #"-"))
       (reduce (fn [acc [from to]]
                 (-> acc
                   (update (keyword from) #(conj % (keyword to)))
                   (update (keyword to) #(conj % (keyword from))))) {})))

(defn- lower-case? [string]
  (every? #(Character/isLowerCase %) string))

(defn- small-caves [path]
  (filter (fn [node]
            (lower-case? (name node))) path))

(defn- branch [graph path]
  (let [last-node (last path)
        visisted-small-caves (set (small-caves path))]
    (for [neighbour (get graph last-node)
          :when (not (visisted-small-caves neighbour))]
      [last-node neighbour])))

(defn part-one
  ([] (part-one (slurp "./twelve/input.txt")))
  ([input]
   (let [graph (parse input)]
     (loop [paths [[:start]]
            completed-paths #{}]
       (let [new-branches (mapcat (fn [path]
                                    (let [branches (branch graph path)]
                                      (map #(concat path (next %)) branches))) paths)
             distinct-paths (filter (fn [branch]
                                      (= :end (last branch))) new-branches)
             all-completed-paths (set (concat completed-paths distinct-paths))]

             (if (seq new-branches)
               (recur (remove all-completed-paths new-branches) all-completed-paths)
               (count completed-paths)))))))

(defn- branch-with-two-small-caves [graph path]
  (let [last-node (last path)
        visisted-small-caves (frequencies (small-caves path))]
    (for [neighbour (get graph last-node)
          :when (and
                  (not= neighbour :start)
                  (or
                    (not (get visisted-small-caves neighbour))
                    (and
                      (get visisted-small-caves neighbour)
                      (every? #{1} (vals visisted-small-caves)))))]
      [last-node neighbour])))

(defn part-two
  ([] (part-two (slurp "./twelve/input.txt")))
  ([input]
   (let [graph (parse input)]
     (loop [paths [[:start]]
            completed-paths #{}]
       (let [new-branches (mapcat (fn [path]
                                    (let [branches (branch-with-two-small-caves graph path)]
                                      (map #(concat path (next %)) branches))) paths)
             distinct-paths (filter (fn [branch]
                                      (= :end (last branch))) new-branches)
             all-completed-paths (set (concat completed-paths distinct-paths))]

         (if (seq new-branches)
           (recur (remove all-completed-paths new-branches) all-completed-paths)
           (count completed-paths)))))))



(comment
(parse example)
(part-one example)
(part-one larger-example)
(part-one even-larger-example)
(part-one)

(part-two example)
(part-two larger-example)
(part-two even-larger-example)
(part-two))
