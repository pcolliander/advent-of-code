(ns ad-of-code.06)

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

;; (def input [10 3 15	10 5 15	5	15 9 2 5 8 5 2 3 6])
;; ;; (def input [0 2 7 0])

; Part 1
(loop [banks input 
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
       (inc distribution-count)))))

; Part 2
(defn- index-of-equal-bank [bank seen-banks]
  (some 
    #(when (zero? (compare (second %) bank)) %) seen-banks))

(defn- get-index-of-first-occurence [bank seen-banks]
  (->> seen-banks
     (map-indexed vector)
     (index-of-equal-bank bank)))

;; (def input [10 3 15	10 5 15	5	15 9 2 5 8 5 2 3 6])
(def input [0 2 7 0])
(loop [banks input 
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
       (inc distribution-count)))))


