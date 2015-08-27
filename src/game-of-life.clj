(ns game-of-life)

(use '[clojure.test :as t])

(defn get-neighbors
  {:test (fn []
           (is (= [[0 1] [1 2] [1 1]]
                  (get-neighbors 0 2 [[0 0 0]
                                      [0 0 0]])))
           (is (= [[0 1] [1 0] [1 1]]
                  (get-neighbors 0 0 [[0 0 0]
                                      [0 0 0]])))
           (is (= (->> (for [x [0 1] y [0 1 2]]
                         [x y])
                       (remove #{[1 1]})
                       (sort))
                  (sort (get-neighbors 1 1 [[0 0 0]
                                            [0 0 0]]))))
           )}
  [x y grid]
  (let [ns [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]
            [(dec x) (dec y)] [(inc x) (inc y)]
            [(dec x) (inc y)] [(inc x) (dec y)]]]
    (filter #(get-in grid %) ns)))

(defn print-board [board]
  (doseq [line board] (println line)))

(defn count-hashed-neighbors
  {:test (fn []
           (is (= 0 (count-hashed-neighbors [] [])))
           (is (= 1 (count-hashed-neighbors [[0 1]] [[" " "#"]])))
           (is (= 2 (count-hashed-neighbors [[0 1] [1 0]] [[" " "#"]
                                                           ["#" " "]]))))}
  [neighbors board]
  (->> (map #(get-in board %) neighbors)
       (remove #{" "})
       (count)))

(defn game [starting-board]
  (let [rows (count starting-board)
        cols (count (first starting-board))]
    (for [x (range 0 rows)
          y (range 1 rows)]
      (let [ns (get-neighbors x y starting-board)
            num-live-neighbors (count-hashed-neighbors ns starting-board)]
        (cond
          (or (= 2 num-live-neighbors) (= 3 num-live-neighbors))


          )))))

(t/run-tests)

