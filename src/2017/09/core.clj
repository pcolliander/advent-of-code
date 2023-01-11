(ns ad-of-code.09.core
  (:require [clojure.string :as str]))

(defn f [input-path]
  (let [input (slurp input-path)
        characters (str/split input #"")]

    (loop [characters characters
           opened-groups 0
           total-score 0
           is-garbage? false
           should-cancel-next-op? false
           previous-char nil
           garbage-count 0]

      (if-let [c (first characters)]
        (case c
          "{" (recur 
                (rest characters)
                (if (not is-garbage?) (inc opened-groups) opened-groups)
                total-score
                is-garbage?
                false
                c
                (if (and (not should-cancel-next-op?) is-garbage?) (inc garbage-count) garbage-count))

          "}" (recur
                (rest characters)
                (if (and (not is-garbage?) (pos? opened-groups)) (dec opened-groups) opened-groups)
                (if (and (not is-garbage?) (pos? opened-groups)) (+ total-score opened-groups) total-score)
                is-garbage?
                false
                c
                (if (and (not should-cancel-next-op?) is-garbage?) (inc garbage-count) garbage-count))

          "<" (recur
                (rest characters)
                opened-groups
                total-score
                true
                false
                c
                (if (and (not should-cancel-next-op?) is-garbage?) (inc garbage-count) garbage-count))
                
          ">" (recur
                (rest characters)
                opened-groups
                total-score
                (if should-cancel-next-op? true false)
                false
                c
                garbage-count)

          "!" (recur
                (rest characters)
                opened-groups
                total-score
                is-garbage?
                (if should-cancel-next-op? false true)
                c
                garbage-count)

          
           (recur
              (rest characters)
              opened-groups
              total-score
              is-garbage?  
              false
              c
              (if (and (not should-cancel-next-op?) is-garbage?) (inc garbage-count) garbage-count)))
        (println "total-score, garbage-count " total-score garbage-count)))))

(f "./src/ad_of_code/09/puzzle-input.txt")

