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

(defn- size-of-literal [accumulator literal-group]
  (+ accumulator (count literal-group)))

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
                           (println :literal/literal literal)
                           {:version-number version
                            :literal literal
                            :len (reduce
                                   size-of-literal
                                   (count header)
                                   literal)})

      (operator? type-id) (let [[length-type-id & operator-tail] binary-tail]
                            (println :length-type-id length-type-id)
                            (case length-type-id
                              0 (let [[length-type-total remaining] (get-length-type operator-tail 15)]
                                   (loop [binary remaining
                                          subpackets []]
                                     (println :operator/length-type-total length-type-total)
                                     (println :operator/subpackets subpackets)

                                     (let [parsed-length (reduce + (map :len subpackets))]
                                       (if (= length-type-total parsed-length)
                                         {:version version
                                          :subpackets subpackets
                                          :len parsed-length}
                                         (let [parsed (parse-packet binary)
                                               [_ rest-of-binary] (split-at (:len parsed) binary)]
                                           (recur
                                             rest-of-binary
                                             (conj subpackets parsed)))))))

                              1 (let [[length-type-total remaining] (get-length-type operator-tail 11)]
                                   (loop [binary remaining
                                          subpackets []]
                                     (println :subpackets subpackets)
                                     (println :operator/length-type-total length-type-total)
                                     (println :operator/subpackets subpackets)

                                     (let [parsed-length (reduce + (map :len subpackets))]
                                       (if (= length-type-total (count subpackets))
                                         {:version version
                                          :subpackets subpackets
                                          :len parsed-length}
                                         (let [parsed (parse-packet binary)
                                               [_ rest-of-binary] (split-at (:len parsed) binary)]
                                           (recur rest-of-binary (conj subpackets parsed))))))))))))

(defn part-one
  ([] (part-one (slurp "./src/2021/day16/input.txt")))
  ([input]
   (let [binary (parse input)]
     (parse-packet binary))))

(comment
;; (extract-literal "101111111000101000")
(parse "D2FE28")
(part-one "D2FE28")
(part-one "38006F45291200")
(part-one "EE00D40C823060"))
