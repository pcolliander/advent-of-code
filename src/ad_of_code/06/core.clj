(ns ad-of-code.06.core
  (:require [clojure.string :as str]))

(defn- is-bank-equal? [bank seen-banks]
  (some #(when (zero? (compare % bank)) %) seen-banks))

(defn- get-index-of-largest-block [input]
 (.indexOf input (reduce max input) ))

(defn- distribute [largest-block banks next-index]
   (loop [largest-block largest-block 
          banks banks 
          next-index next-index]

     (if (pos? largest-block)
       (recur 
         (dec largest-block) 
         (update banks (mod next-index (count banks)) inc) 
         (inc next-index))
       banks)))

; Part 1
(defn count-distribution-cycles [input-path]
  (let [input (slurp input-path)
        memory-banks-as-strings (str/split input #"\s")
        memory-banks (map #(Integer/parseInt %) memory-banks-as-strings) ]

    (println "input "input )
    (println "memory-banks-as-strings " memory-banks-as-strings)

  (loop [banks (vec memory-banks)
         seen-banks [] 
         distribution-count 1]

    (let [index-of-largest-block (get-index-of-largest-block banks)
         largest-block (get banks index-of-largest-block)
         updated-banks (assoc banks index-of-largest-block 0)
         distributed-banks (distribute largest-block updated-banks (inc index-of-largest-block))]

     (if (is-bank-equal? distributed-banks seen-banks)
       (println {:banks banks :count distribution-count})
       (recur 
         distributed-banks 
         (conj seen-banks distributed-banks) 
         (inc distribution-count)))))))

(count-distribution-cycles "./src/ad_of_code/06/puzzle-input.txt")

; Part 2
(defn- index-of-equal-bank [bank seen-banks]
  (some 
    #(when (zero? (compare (second %) bank)) %) seen-banks))

(defn- get-index-of-first-occurence [bank seen-banks]
  (->> seen-banks
     (map-indexed vector)
     (index-of-equal-bank bank)))

(defn how-many-cycles-in-infinite-loop [input-path]
  (let [input (slurp input-path)
        memory-banks-as-strings (str/split input #"\s")
        memory-banks (map #(Integer/parseInt %) memory-banks-as-strings) ]

    (loop [banks (vec memory-banks)
           seen-banks [] 
           distribution-count 1]

      (let [index-of-largest-block (get-index-of-largest-block banks)
           largest-block (get banks index-of-largest-block)
           updated-banks (assoc banks index-of-largest-block 0)
           distributed-banks (distribute largest-block updated-banks (inc index-of-largest-block))]

       (if (is-bank-equal? distributed-banks seen-banks)
         (println "loop length " (dec (- distribution-count (first (get-index-of-first-occurence distributed-banks seen-banks))))) ; taking -1 since I didn't put the initial memory bank on the list.
         (recur 
           distributed-banks 
           (conj seen-banks distributed-banks) 
           (inc distribution-count)))))))

(how-many-cycles-in-infinite-loop "./src/ad_of_code/06/puzzle-input.txt")


