(ns ad-of-code.12.core
  (:require [clojure.string :as str]))

(defn- get-links [string]
  (let [links-as-string (str/replace (re-find #"<->.*$" string)  "<->" "")]
    (-> links-as-string
      (str/trim)
      (str/replace " " "")
      (str/split  #","))))

(defn- build-graph [string]
  (loop [input-lines (str/split-lines string) graph {}]
    (if-let [line (first input-lines)]
      (let [node (Integer/parseInt (str (nth (str/split line #" ") 0)))
            links (map #(Integer/parseInt %) (get-links line))]
      (recur 
        (rest input-lines)
        (assoc graph node links)))
      graph)))

(defn- is-connected? [link link-to-check graph]
  (if (= link link-to-check)
    true
    (loop [stack (into '() (get graph link))
           seen-nodes #{}]
      (if-let [next-node (first stack)]
        (if (= next-node link-to-check)
          true
          (if (get seen-nodes next-node)
            (recur
              (rest stack)
              seen-nodes)
            (recur
              (concat (rest stack) (get graph next-node))
              (conj seen-nodes next-node))))
          false))))

(defn how-many-programs [input-path] 
  (let [input (slurp input-path)
        graph (build-graph input) ]
    
    (loop [programs (keys graph)
           counter 0]
      (if-let [program (first programs)]
        (recur 
          (rest programs)
          (if (is-connected? program 0 graph) (inc counter) counter))
        (do
          (println "counter" counter)
          counter )))))

(defn- dfs [program graph]
  (loop [stack (into '() (get graph program))
         seen-nodes #{}]

    (if-let [next-node (first stack)]
       (if (get seen-nodes next-node)
          (recur
            (rest stack)
             seen-nodes)
          (recur
            (concat (rest stack) (get graph next-node))
            (conj seen-nodes next-node)))
          seen-nodes)))

; Part 2
(defn how-many-groups [input-path]
  (let [input (slurp input-path)
        graph (build-graph input) ]

    (loop [programs (keys graph)
           group-counter 0
           seen-programs #{}]

      (if-let [program (first programs)]
        (if (get seen-programs program)
          (recur
            (rest programs)
            group-counter
            seen-programs)
          (recur 
            (rest programs)
            (inc group-counter)
            (apply conj seen-programs (dfs program graph))) ) 
        group-counter))))

(how-many-groups "./src/ad_of_code/12/test-input.txt")
(how-many-groups "./src/ad_of_code/12/puzzle-input.txt")

