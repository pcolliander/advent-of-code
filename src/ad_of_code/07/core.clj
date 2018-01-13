(ns ad-of-code.07.core
  (:require [clojure.string :as str]))

(defn- get-head [string]
  (first (str/split string #" ")))

(defn- get-links [string]
  (let [links-as-string (str/replace (re-find #"->.*$" string)  "->" "")]
    (-> links-as-string
        (str/trim)
        (str/split #", "))))

(defn- get-programs-with-links [input-lines]
 (let [string-with-links (filter #(> (.indexOf % ",") 0) input-lines)]
   (loop [string-with-links (vec string-with-links)
          programs-with-links {}]
     (if-let [string (first string-with-links)]
       (recur
         (rest string-with-links)
         (assoc programs-with-links (get-head string) (get-links string)))
       programs-with-links))))

(defn find-topmost-program [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        top-programs (get-programs-with-links input-lines)]

    (loop [top-programs-keys (keys top-programs)]
      (if-let [program-key (first top-programs-keys)]
        (if (nil? (some #(when (= % program-key) %) (flatten (vals top-programs))))
          (do
            ;; (println "result is " program-key)
            program-key)
          (recur (rest top-programs-keys)) )))))

;; (find-topmost-program "./src/ad_of_code/07/puzzle-input.txt")

; Part 2
(defn- get-weight [string]
  (let [number-as-string (-> (second (str/split string #" "))
                            (str/replace #"\(" "")
                            (str/replace #"\)" ""))]
    (Integer/parseInt number-as-string)))

(defn get-weight-by-program [input-lines]
  (loop [programs-by-weight {}
        input-lines input-lines]
    (if-let [string (first input-lines)]
      (let [head (get-head string)
            weight (get-weight string)]
      (recur    
        (assoc programs-by-weight head weight)
        (rest input-lines)) )
      programs-by-weight )))

(defn get-total-weight-of-program [program weight-by-program programs-with-links]
  (loop [stack (into '() (get programs-with-links program) )
         total-weight (get weight-by-program program)] 

    (if (not= 0 (count stack))
      (let [link (first stack)]
        (if-let [links (get programs-with-links link)]
          (recur 
            (concat (rest stack) links)
            (+ total-weight (get weight-by-program link)))
          (recur 
            (rest stack)
            (+ total-weight (get weight-by-program link)))))

      {:program program :weight total-weight})))

(defn find-weight [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        programs-with-links (get-programs-with-links input-lines)
        weight-by-program (get-weight-by-program input-lines)
        top-program (find-topmost-program input-path)]

    (loop [program top-program]
      (let [links (get programs-with-links program)
            uneven-link (->> links
              (map #(get-total-weight-of-program % weight-by-program programs-with-links))
              (group-by :weight)
              (filter #(= 1 (count (second %))))
              (flatten))]

        (if (empty? uneven-link)
          (println "the unbalanced program is" program)
          (recur
            (:program (second uneven-link)))) ))))

(find-weight "./src/ad_of_code/07/puzzle-input.txt")
;; (find-weight "./src/ad_of_code/07/test-input.txt")

