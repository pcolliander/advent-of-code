(ns ad-of-code.03.core)

; this creates a sequence like 4321012343210 (if 4 is the number of grids away from the most-inner grid).
; used to get the offset from the middle number.
(defn- create-seq [should-inc? coll max-numb end-of-sequence]
  (let [last-item (or (last coll) 0)]
   (if (< (count coll) end-of-sequence)
     (if should-inc?
      (create-seq (not= (+ last-item 1) max-numb)  (conj coll (+ last-item 1)) max-numb end-of-sequence)
      (create-seq (= (- last-item 1) 0) (conj coll (- last-item 1)) max-numb end-of-sequence))
    coll)))

(defn- calc-number-of-steps [target current]
  (let [steps-from-last-item-in-grid (- (:last-number current) target) 
        max-numb (:#s-of-8s current)
        my-seq (create-seq false [max-numb] max-numb (- (:last-number current) (:prev-number current)))
        offset (get my-seq steps-from-last-item-in-grid)]

    (println "result " (+ max-numb offset)) ))

(defn calc-ranges [target step current]
  (let [last-number (:last-number current)]

    (if (>= last-number target)
      (calc-number-of-steps target current)
      (calc-ranges target (+ step 8) {:prev-number last-number 
                                     :last-number (+ last-number step) 
                                     :#s-of-8s (+ (:#s-of-8s current) 1)}))))

(calc-ranges 265149 8 {:last-number 1 :#s-of-8s 0})



