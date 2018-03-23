(ns ad-of-code.13.core
  (:require [clojure.string :as str]))

(defn- scanner-in-top-of-layer? [layer-range picosecond]
  (let [scanned-layers (cycle (concat (range 1 (inc layer-range)) (reverse (range 2 layer-range))))
        current-position (nth scanned-layers picosecond)]

    (= current-position 1)))

(defn- create-range-by-layer [lines]
  (loop [range-by-layer {} 
         lines lines
         items-count 0]
    (if-let [line (first lines)]
      (let [layer (Integer/parseInt (subs line 0 (.indexOf line ":")))
            layer-range (Integer/parseInt (str/trim (subs line (inc (.indexOf line ":")))))]
          (recur 
            (assoc range-by-layer layer layer-range)
            (rest lines)
            (inc items-count)))

      range-by-layer)))

(get {0 3, 1 2, 4 4, 6 4} 0)

(reduce max '(0 1 4 5))


(defn f [input-path]
  (let [input (slurp input-path)
        range-by-layer (create-range-by-layer (str/split-lines input))
        items-count (count range-by-layer)
        layers (range 0 (inc (reduce max (keys range-by-layer))))]

    (loop [picosecond 0
           layers layers
           severity 0]

      (if-let [layer (first layers)]
        (let [layer-range (get range-by-layer layer )
              caught? (and layer-range (scanner-in-top-of-layer? layer-range picosecond))] ; and to remove nil (no range)

          (recur
            (inc picosecond)
            (rest layers)
            (if caught? 
              (+ severity (* layer-range layer))  ; range and layer are the same?
              severity)))
        
        (do
          (println "severity is: " severity)
          severity)))))

;; (f "./src/ad_of_code/13/test-input.txt")
(f "./src/ad_of_code/13/puzzle-input.txt")


