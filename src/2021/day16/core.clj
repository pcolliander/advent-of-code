(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.data.priority-map :refer [priority-map]]))

(def hexa->binary
  {:0 [0 0 0 0]
   :1 [0 0 0 1]
   :2 [0 0 1 0]
   :3 [0 0 1 1]
   :4 [0 1 0 0]
   :5 [0 1 0 1]
   :6 [0 1 1 0]
   :7 [0 1 1 1]
   :8 [1 0 0 0]
   :9 [1 0 0 1]
   :A [1 0 1 0]
   :B [1 0 1 1]
   :C [1 1 0 0]
   :D [1 1 0 1]
   :E [1 1 1 0]
   :F [1 1 1 1]})

(defn- parse [input]
  (->> input
       (map #(-> % str keyword))
       (mapcat hexa->binary))) 

(defn- literal? [id] (= "100" id))
(def operator? (complement literal?))   

;; (defn- subpackets-in-bit-size [subpackets]
;;   (let [literal-value-bits (map (fn [values]
;;                                   (->> values
;;                                        (map count)
;;                                        (reduce +)))
;;                                 subpackets)
;;         header-bits (* 6 (count subpackets))]
;;     (reduce + (flatten [literal-value-bits header-bits]))))
             
(defn- extract-literal [package]
  (loop [package package
         groups []]
    (let [[group remaining] (split-at 5 package)]
      (if (zero? (first group))
        (let [full-packet (conj groups group)]
          [full-packet remaining])
        (recur remaining (conj groups group))))))

(defn- size-of-literal [literal]
  (reduce (fn [acc n]
            (+ acc (count n))) 0 literal))

(comment
  (let [binary (parse "D2FE28")
        [header tail] (split-at 6 binary)
        [literal remaining] (extract-literal tail)]

    (size-of-literal literal)))


(defn- get-length-type [binary length]
  (let [[head tail] (split-at length binary)
        length-type (string/join head)]
    [(Long/parseLong length-type 2) tail]))

(defn- parse-packet [binary]
  (let [[header binary-tail] (split-at 6 binary)
        version (->> header (take 3) string/join)
        type-id (->> header (take-last 3) string/join)]

    (println :version version)
    (println :binary binary)
    (println :type-id type-id)

    (cond
      (literal? type-id) (let [[literal rest-of-binary] (extract-literal binary-tail)]      
                           {:version-number version
                            :literal literal
                            :len (size-of-literal literal)})

      (operator? type-id) (let [[length-type-id & operator-tail] binary-tail]
                            (println :length-type-id length-type-id)
                            (case length-type-id
                              \0 (let [[length-type-total remaining] (get-length-type operator-tail 15)]
                                   (loop [binary remaining
                                          subpackets []
                                          n 0
                                          ]
                                     (println :length-type-total length-type-total)
                                     (println :subpackets subpackets)
                                     (println :remaining remaining)
                                     (if (or 
                                           (= n 10)
                                           #_(= length-type-total (subpackets-in-bit-size subpackets)))
                                       {:version version}
                                       (let [parsed (parse-packet binary)]
                                         (recur binary (conj subpackets parsed) (inc n))))))

                              \1 (let [[length-type-total remaining] (get-length-type operator-tail 11)]
                                   (loop [binary remaining
                                          subpackets []
                                          n 0
                                          ]
                                     (println :subpackets subpackets)
                                     (if (or
                                           (= length-type-total (count subpackets))
                                           (= n 10))
                                       {:version version}
                                       (let [parsed (parse-packet binary)]
                                         (recur binary (conj subpackets parsed) (inc n)))))))))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day16/input.txt")))
  ([input]
   (let [binary (parse input)]
     (parse-packet binary))))

(comment
;; (extract-literal "101111111000101000")
(parse "D2FE28")
(part-one "D2FE28")
(part-one "38006F45291200"))
