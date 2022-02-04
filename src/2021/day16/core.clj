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
             
(defn- extract-literal [package]
  (loop [package package
         groups []]
    (let [[group remaining] (split-at 5 package)]
      (if (zero? (first group))
        (let [full-packet (conj groups group)]
          [full-packet remaining])
        (recur remaining (conj groups group))))))

(defn- size-of-literal [accumulator literal-group]
  (+ accumulator (count literal-group)))

(defn- get-length-type [binary length]
  (let [[head tail] (split-at length binary)
        length-type (string/join head)]
    [(Long/parseLong length-type 2) tail]))

(Long/parseLong "00000000010" 2)
(Long/parseLong "000000000010110" 2)

(defn- parse-packet [binary]
  (let [[header binary-tail] (split-at 6 binary)
        version (->> header (take 3) string/join)
        type-id (->> header (take-last 3) string/join)
        header-size (count header)]
    (cond
      (literal? type-id) (let [[literal rest-of-binary] (extract-literal binary-tail)]      
                           {:version-number version
                            :version-sum (Long/parseLong version 2)
                            :literal literal
                            :len (reduce
                                   size-of-literal
                                   header-size
                                   literal)})

      (operator? type-id) (let [[length-type-id & operator-tail] binary-tail]
                            (case length-type-id
                              0 (let [[length-type-total remaining] (get-length-type operator-tail 15)]
                                   (loop [binary remaining
                                          subpackets []]
                                     (let [parsed-length (reduce + (map :len subpackets))]
                                       (if (= length-type-total parsed-length)
                                         {:version version
                                          :version-sum (+ (Long/parseLong version 2) (apply + (map :version-sum subpackets)))
                                          :len (+ header-size 16 parsed-length)
                                          :subpackets subpackets}
                                         (let [parsed (parse-packet binary)
                                               [_ rest-of-binary] (split-at (:len parsed) binary)]
                                           (recur
                                             rest-of-binary
                                             (conj subpackets parsed)))))))

                              1 (let [[length-type-total remaining] (get-length-type operator-tail 11)]
                                   (loop [binary remaining
                                          subpackets []]
                                     (let [parsed-length (reduce + (map :len subpackets))]
                                       (if (= length-type-total (count subpackets))
                                         {:version version
                                          :version-sum (+ (Long/parseLong version 2) (apply + (map :version-sum subpackets)))
                                          :len (+ header-size 12 parsed-length)
                                          :subpackets subpackets}
                                         (let [parsed (parse-packet binary)
                                               [_ rest-of-binary] (split-at (:len parsed) binary)]
                                           (recur rest-of-binary (conj subpackets parsed))))))))))))

(Long/parseLong "000" 2)
(Long/parseLong "011" 2)
(defn part-one
  ([] (part-one (slurp "./src/2021/day16/input.txt")))
  ([input]
   (let [binary (parse input)
         result (parse-packet binary)]

     (:version-sum result))))

(comment
(parse "D2FE28")
(parse "620080001611562C8802118E34")

(part-one "D2FE28")
(part-one "38006F45291200")
(part-one "EE00D40C823060")
(part-one "8A004A801A8002F478")
(part-one "620080001611562C8802118E34")
(part-one "C0015000016115A2E0802F182340")
(part-one "A0016C880162017C3686B18A3D4780"))
