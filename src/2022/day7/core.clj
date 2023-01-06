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
  (str "/" (string/join "/" (next dir))))

(defn- process [n]
  (loop [commands n
         filesystem {}
         dir []]
    (let [[command tail] (split-with #(not= % "$") (next commands))]
      (if (seq command)
        (case (first command)
          "cd" (case (second command)
                 ".." (recur tail filesystem (pop dir))
                 "/"  (recur tail filesystem ["/"])
                 (recur tail filesystem (conj dir (second command))))
          "ls"   (recur tail (assoc filesystem (filepath dir)
                                    (->> (next command) (partition 2) (map (fn [[f s]]
                                                                             (if (= f "dir")
                                                                               [f (filepath (conj dir s))]
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
             {})))

(defn part-one
  ([] (part-one (slurp "./src/2022/day7/input.txt")))
  ([input]
   (let [filesystem (-> input parse process)]
     (->> (filesystem-with-size filesystem)
          vals
          (map (comp (partial reduce +) flatten))
          (filter (partial >= 100000))
          (reduce + 0)))))

(defn part-two
  ([] (part-two (slurp "./src/2022/day7/input.txt")))
  ([input]
   (let [filesystem-with-sz (-> input parse process filesystem-with-size)
         total-used (apply max (->> filesystem-with-sz vals (map (comp (partial reduce +) flatten))))
         unused-space (- 70000000 total-used)
         needed-space (- 30000000 unused-space)]

     (->> filesystem-with-sz
          (reduce (fn [acc [path sizes]]
                    (into acc [[path (apply + (flatten sizes))]]))
                  {})
          (map identity)
          (sort-by second)
          (drop-while (fn [[f s]]
                        (<= s needed-space)))
          (first)
          (second)))))

(comment
  (part-one example)
  (part-one)
  (part-two example)
  (part-two))

