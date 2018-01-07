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
   (loop [string-with-links (vec string-with-links)
          top-programs {}]
     (if-let [string (first string-with-links)]
       (recur
         (rest string-with-links)
         (conj top-programs (hash-map (get-head string) (get-links string))))
       top-programs))))

(defn find-topmost-program [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        top-programs (map-input input-lines)]

    (loop [top-programs-keys (keys top-programs)]
      (if-let [program-key (first top-programs-keys)]
        (if (nil? (some #(when (= % program-key) %) (flatten (vals top-programs))))
          (println "result is " program-key)
          (recur (rest top-programs-keys)) )))))

(find-topmost-program "./src/ad_of_code/07/puzzle-input.txt")

