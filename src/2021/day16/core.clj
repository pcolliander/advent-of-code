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

(defn- compute-literal [literal]
  (Long/parseLong (string/join
                    (map #(string/join (drop 1 %)) literal)) 2))

(defn- extract-literal [package]
  (loop [package package
         groups []]
    (let [[group remaining] (split-at 5 package)]
      (if (zero? (first group))
        (conj groups group)
        (recur remaining (conj groups group))))))

(defn- size-of-literal [accumulator literal-group]
  (+ accumulator (count literal-group)))

(defn- get-length-type [binary length]
  (let [[head tail] (split-at length binary)
        length-type (string/join head)]
    [(Long/parseLong length-type 2) (vec tail)]))

(defn- calculate-value [type-id subpackets]
  (let [values (map :value subpackets)]
    (case (Long/parseLong type-id 2)
      0 (apply + values)
      1 (apply * values)
      2 (apply min values)
      3 (apply max values)
      5 (if (> (first values) (second values))
          1
          0)
      6 (if (< (first values) (second values))
          1
          0)
      7 (if (apply = values) 1 0))))

(defn- parse-packet [binary]
  (let [[header binary-tail] (split-at 6 binary)
        version (Long/parseLong (->> header (take 3) string/join) 2)
        type-id (->> header (take-last 3) string/join)
        header-size (count header)]
    (cond
      (literal? type-id) (let [literal (extract-literal binary-tail)]      
                           {:version-sum version
                            :value (compute-literal literal)
                            :len (reduce
                                   size-of-literal
                                   header-size
                                   literal)})

      (operator? type-id) (let [[length-type-id & package] binary-tail]
                            (case length-type-id
                              0 (let [[length-type-total remaining] (get-length-type package 15)]
                                   (loop [binary remaining
                                          subpackets []]
                                     (let [parsed-length (reduce + (map :len subpackets))]
                                       (if (= length-type-total parsed-length)
                                         {:version-sum (+ version (apply + (map :version-sum subpackets)))
                                          :value (calculate-value type-id subpackets)
                                          :len (+ header-size 16 parsed-length)
                                          :subpackets subpackets}
                                         (let [parsed (parse-packet binary)
                                               rest-of-binary (subvec binary (:len parsed))]
                                           (recur
                                             rest-of-binary
                                             (conj subpackets parsed)))))))

                              1 (let [[length-type-total remaining] (get-length-type package 11)]
                                   (loop [binary remaining
                                          subpackets []]
                                     (let [parsed-length (reduce + (map :len subpackets))]
                                       (if (= length-type-total (count subpackets))
                                         {:version-sum (+ version (apply + (map :version-sum subpackets)))
                                          :value (calculate-value type-id subpackets)
                                          :len (+ header-size 12 parsed-length)
                                          :subpackets subpackets}
                                         (let [parsed (parse-packet binary)
                                               rest-of-binary (subvec binary (:len parsed))]
                                           (recur rest-of-binary (conj subpackets parsed))))))))))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day16/input.txt")))
  ([input]
   (let [binary (parse input)
         result (parse-packet binary)]
     (:version-sum result))))

(defn part-two
  ([] (part-two (slurp "./src/2021/day16/input.txt")))
  ([input]
   (let [binary (parse input)
         result (parse-packet binary)]
     (:value result))))

(comment
(parse "D2FE28")
(parse "620080001611562C8802118E34")

(part-one "D2FE28")
(part-one "38006F45291200")
(part-one "EE00D40C823060")
(part-one "8A004A801A8002F478")
(part-one "620080001611562C8802118E34")
(part-one "C0015000016115A2E0802F182340")
(part-one "A0016C880162017C3686B18A3D4780")
(part-one)

(part-two "C200B40A82") ; sum
(part-two "04005AC33890") ; product
(part-two "880086C3E88112") ; min
(part-two "CE00C43D881120") ; max

(part-two "D8005AC2A8F0")
(part-two "F600BC2D8F")
(part-two "9C005AC2F8F0")
(part-two "9C0141080250320F1802104A08")
(part-two))
