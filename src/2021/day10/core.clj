(ns advent-of-code-2021.core
  (:require [clojure.edn :as edn]
             [clojure.set :as cset]
             [clojure.string :as string]
             [clojure.pprint :as pp]))

(def example "[({(<(())[]>[[{[]{<()<>>
             [(()[<>])]({[<{<<[]>>(
             {([(<{}[<>[]}>{[]{[(<()>
             (((({<>}<{<{<>}{[]{[]{}
             [[<[([]))<([[{}[[()]]]
             [{[{({}]{}}([{[{{{}}([]
             {<[[]]>}<{[{[{[]{()[[[]
             [<(<(<(<{}))><([]([]()
             <{([([[(<>()){}]>(<<{{
             <{([{{}}[<[[[<>{}]]]>[]]")

(defn- parse [input]
  (->> (string/split-lines input)
       (map string/trim)))

(def points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def matching-chars
  {\] \[
   \} \{
   \) \(
   \> \<})

; keep a stack of all ops. Pop items once they're matched.
(defn- parse-line [line]
  (loop [line line
         stack []]
    (if-let [character (first line)]
        (cond
          (some #{\[\(\{\<} [character]) (recur
                                           (next line)
                                           (conj stack character))

          (= (peek stack)
             (get matching-chars character)) (recur (next line)
                                                    (pop stack))

          :else (get points character))
        0)))

(defn part-one
  ([] (part-one (slurp "./ten/input.txt")))
  ([input]
   (let [subsystem (parse input)]
     (->> subsystem
          (map parse-line)
          (reduce + 0)))))

(defn- complete-line [line]
  (loop [line line
         stack []]
    (if-let [character (first line)]
        (cond
          (some #{\[\(\{\<} [character]) (recur
                                           (next line)
                                           (conj stack character))

          (= (peek stack)
             (get matching-chars character)) (recur (next line)
                                                    (pop stack)))
        (map (fn [opening-char]
               (get (cset/map-invert matching-chars) opening-char))
               (reverse stack)))))

(defn middle [[fst & rst]]
  (if-not rst fst
    (recur (butlast rst))))

(defn score-completion [line]
  (map (fn [closing-char]
         (case closing-char
           \) 1
           \] 2
           \} 3
           \> 4)) line))

(defn part-two
  ([] (part-two (slurp "./ten/input.txt")))
  ([input]
   (let [subsystem (parse input)]
     (->> subsystem
          (remove #(pos? (parse-line %)))
          (map complete-line)
          (map score-completion)
          (map (fn [points]
                 (reduce (fn [acc point]
                           (+ (* acc 5) point)) 0 points)))
          (sort <)
          middle))))

(comment
(part-one example)
(part-one)
(part-two example)
(part-two))

