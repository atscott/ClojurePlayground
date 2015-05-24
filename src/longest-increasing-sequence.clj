(defn get-neighbors [x y grid]
  (let [ns [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]]]
    (filter #(get-in grid %) ns)))

(def memo-increasing #(0))
(defn find-max [x y grid]
  (let [ns (get-neighbors x y grid)
        increasing-neighbors (filter #(> (get-in grid %) (get-in grid [x y])) ns)]
    (if (empty? increasing-neighbors)
      1
      (apply max (map #(inc (memo-increasing (first %) (last %) grid)) increasing-neighbors)))))
(def memo-increasing (memoize find-max))

(defn longest-increasing [grid]
  (let [depths (for [x (range 0 (count grid))
                     y (range 0 (count (first grid)))]
                 (memo-increasing x y grid))]
    (apply max depths)))

(longest-increasing [[1 2 2 4]
                     [4 5 4 1]
                     [7 2 6 7]])
