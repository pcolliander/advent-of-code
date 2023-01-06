(ns advent-of-code-2022.core
   (:require [clojure.string :as string]
             [clojure.set :as s]))

(def example
"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parse [input] 
  (-> input
    (string/replace #"\n" " ")
    (string/split #" " )))
                    
(defn filepath [dir]
  (str "/" (string/join "/" dir)))

(defn- process [n]
  (loop [commands n
         filesystem {}
         dir []]
    (let [[command tail] (split-with #(not= % "$") (next commands))
          filepath (filepath (next dir))]
      (if (seq command)
        (case (first command)
          "cd" (case (second command)
                 ".." (recur tail filesystem (pop dir))
                 "/"  (recur tail filesystem ["/"])
                 (recur tail filesystem (conj dir (second command))))
          "ls"   (recur tail (assoc filesystem filepath
                                    (->> (next command) (partition 2) (map (fn [[f s]]
                                                                             (if (= f "dir")
                                                                               [f (str filepath "/" s)]
                                                                               [f s]))))) dir))
        filesystem))))

(defn- size [filesystem [f s]]
  (if (= f "dir")
    (->> (get filesystem s) (map (partial size filesystem)) flatten)
    (parse-long f)))

(defn- filesystem-with-size [filesystem]
  (->> filesystem
    (reduce-kv (fn [acc k v]
               (assoc acc k (map (partial size filesystem) v)))
             {})
    vals
    (map (comp (partial reduce +) flatten))
    (filterv (partial >= 100000))
    (reduce + 0)))

(defn part-one
  ([] (part-one (slurp "./src/2022/day7/input.txt")))
  ([input]
   (let [filesystem (-> input parse process)]
     (filesystem-with-size filesystem))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))
