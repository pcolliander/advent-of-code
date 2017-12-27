(ns ad-of-code.05)

(declare ad-input)
(def input [0 3 0 1 -3])

; Part 1
(loop [steps 1 current-index 0 array ad-input]
	(let [steps-to-jump (get array current-index)
				new-index (+ current-index steps-to-jump)]

		(if (get array new-index)
			(recur (inc steps) new-index (update array current-index inc))
			steps)))

; Part 2
(loop [steps 1 current-index 0 array ad-input]
	(let [steps-to-jump (get array current-index)
				next-index (+ current-index steps-to-jump)]

		(if (get array next-index)
			(recur
				(inc steps)
				next-index
				(if (>= steps-to-jump 3)
					(update array current-index dec)
					(update array current-index inc)))
			steps)))

