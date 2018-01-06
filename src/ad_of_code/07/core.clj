(ns ad-of-code.07.core
  (:require [clojure.string :as str]))

(defn- get-head [string]
  (first (str/split string #" ")))

(defn- get-links [string]
  (let [links-as-string (str/replace (re-find #"->.*$" string)  "->" "")]
    (-> links-as-string
        (str/trim)
        (str/split #", "))))

(defn- map-input [input-lines]
  (let [string-with-links (filter #(> (.indexOf % ",") 0) input-lines)]
    (loop [n (count string-with-links)
           top-programs {}]

      (if (= n 0)
        top-programs
        (recur
          (dec n)
          (conj top-programs (hash-map (get-head (nth string-with-links (dec n) )) (get-links (nth string-with-links (dec n) )))))))))

(defn find-topmost-program [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        top-programs (map-input input-lines)]

    (for [k (keys top-programs)]
      (if (nil? (some #(when (= % k) %) (flatten (vals top-programs))))
        (println "result is: " k)) )))

(find-topmost-program "./src/ad_of_code/07/puzzle-input.txt")

