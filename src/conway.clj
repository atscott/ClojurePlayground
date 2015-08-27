(ns game-of-life)

(use '[clojure.test :as t])

(defn get-neighbors
  {:test (fn []
           (is (= (sort [[0 1] [1 2] [1 1]])
                  (sort (get-neighbors [0 2] [[0 0 0]
                                              [0 0 0]]))))
           (is (= [[0 1] [1 0] [1 1]]
                  (get-neighbors [0 0] [[0 0 0]
                                        [0 0 0]])))
           (is (= (->> (for [x [0 1] y [0 1 2]]
                         [x y])
                       (remove #{[1 1]})
                       (sort))
                  (sort (get-neighbors [1 1] [[0 0 0]
                                              [0 0 0]]))))
           )}
  [[x y] grid]
  (->> (for [dx (range -1 2) dy (range -1 2)
             :when (not (and (= 0 dx) (= 0 dy)))]
         [(+ x dx) (+ y dy)])
       (filter #(get-in grid %))))

(defn print-board [board]
  (doseq [line board] (println line)))

(defn count-hashed-neighbors
  {:test (fn []
           (is (= 0 (count-hashed-neighbors [0 1] [[" " "#"]])))
           (is (= 2 (count-hashed-neighbors [0 0] [[" " "#"]
                                                   ["#" " "]]))))}
  [coordinate board]
  (->> (map #(get-in board %) (get-neighbors coordinate board))
       (remove #{" "})
       (count)))

(defn conway-rules [[x y] board]
  (let [alive-neighbors (count-hashed-neighbors [x y] board)]
    (cond
      (< alive-neighbors 2) " "
      (= alive-neighbors 3) "#"
      (> alive-neighbors 3) " "
      :else (get-in board [x y])
      ))
  )

(defn get-next-board
  {:test (fn []
           (is (= [[" " "#" " "]] (get-next-board [["#" "#" "#"]])))
           (is (= [[" " "#" " "]
                   [" " "#" " "]] (get-next-board [[" " "#" " "]
                                                   ["#" " " "#"]]))))}
  [board]
  (->> (for [x (range 0 (count board))
             y (range 0 (count (first board)))]
         (conway-rules [x y] board))
       (partition (count (first board)))))

(defn play
  [game-info]
  (cond (> (:rounds game-info) 0)
        (do
          (println (str "rounds left: " (:rounds game-info)))
          (print-board (:board game-info))
          (recur {:board  (get-next-board (:board game-info))
                 :rounds (dec (:rounds game-info))}))
        :else "Done"))


(play {:board  [[" " "#" " "]
                ["#" " " "#"]]
       :rounds 5})

(t/run-tests)
