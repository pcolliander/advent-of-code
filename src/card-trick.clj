(defn cards-color [color]
  (->>
    (flatten [(range 1 10) "knight" "queen" "king" "ace"])
    (map (fn [number]
           [number color]))))

; Replace the shuffle function in your first solution with the reproducible one. I will show you my solution below:
(defn- shuffle'
    [seed coll]
      (let [rng (java.util.Random. seed)
                    rnd #(do % (.nextInt rng))]
            (sort-by rnd coll)))

(defn- take-rand'
    ([n coll]
          (->> coll shuffle (take n)))
      ([n coll seed]
            (->> coll (shuffle' seed) (take n))))

(defn divide-into-piles [cards]
  {:pile-one   (take-nth 3 cards)
   :pile-two   (take-nth 3 (drop 1 cards))
   :pile-three (take-nth 3 (drop 2 cards))})

(defn play-game [cards round]
  (let [{:keys [pile-one pile-two pile-three]} (divide-into-piles cards)]

    (println)
    (println :pile-one pile-one)
    (println)
    (println :pile-two pile-two)
    (println)
    (println :pile-three pile-three)

    (if (= round 3)
      (do
        (println "Your card is: "  (nth (reverse cards) 10))
        (println "all cards "  (reverse cards)))

      (do
        (println)
        (println "In which pile is your card?")
        (let [input (read-line)]
          (case input
            "1" (play-game (concat pile-two pile-one pile-three) (inc round))
            "2" (play-game (concat pile-one pile-two pile-three) (inc round))
            "3" (play-game (concat pile-two pile-three pile-one) (inc round))))))))


(defn generate-cards []
  (let [cards (mapcat cards-color ["spades" "heart" "clubs" "diamond"])
        randomly-selected-cards (take-rand' 21 cards)]

    (println)
    (println "select a card, and press '1' when you've got a card in mind.")
    (println)
    (println :randomly-selected-cards randomly-selected-cards)

    (let [input (read-line)]
      (when (= input "1")
        (play-game randomly-selected-cards 0)))))

(generate-cards)

