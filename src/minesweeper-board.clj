(defn generate-board [rows cols mines]
  (->> (repeat mines true)
       (concat (repeat (- (* rows cols) mines) false))
       (shuffle)
       (partition cols)))

(generate-board 4 5 6)