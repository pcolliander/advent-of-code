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
            (println "result is " program-key)
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

(defn- get-weight-of-program-and-its-children [program total-weight programs-with-links weight-by-program] 
  (println "program " program)
  (println "total-weight " total-weight)
  (println "get programs-with-links program " (get programs-with-links program))
  (println "get weight-by-program program" (get weight-by-program program))

  (if-let [links-of-program (get programs-with-links program)]
    (loop [links-of-program links-of-program
           total-weight total-weight]

      (if-let [link (first links-of-program)]
        (let [weight (get-weight-of-program-and-its-children link total-weight programs-with-links weight-by-program)
              weight2 (+ weight (get weight-by-program link)) ]

          (println "weight here" weight)
          (println "(+ total-weight weight)" (+ total-weight weight2))
          (recur 
            (rest links-of-program)
            (+ total-weight weight2)))))


      (get weight-by-program program)))  ; if no links, just take the weight of the program.

(defn find-weight [input-path]
  (let [file (slurp input-path)
        input-lines (str/split-lines file)
        programs-with-links (get-programs-with-links input-lines)
        weight-by-program (get-weight-by-program input-lines)
        top-program (find-topmost-program  input-path)
        links-of-the-top-program (get programs-with-links top-program) ]

    (println "links-of-the-top-program " links-of-the-top-program )

    (println "top-program " top-program)
             
    (println " result " (->> links-of-the-top-program
      (map (fn [program] 
      (get-weight-of-program-and-its-children program 0 programs-with-links weight-by-program)) )
      (doall)
      
      ))))


    ;; (loop [top-programs-keys (keys programs-with-links)]
    ;;   (if-let [program (first top-programs-keys)]
    ;;     (let [links-of-program (get programs-with-links program)
    ;;           has-uneven-link? (compare-weight-of-links program links-of-program weight-by-program programs-with-links) ]
    ;;
    ;;       (if has-uneven-link?
    ;;         (println "result is one of the children for" program) ; (reduce - [weight weight2])
    ;;         (recur (rest top-programs-keys))) )))))

(find-weight "./src/ad_of_code/07/puzzle-input.txt")

