(defn unvisited-increasing-neighbors [neighbors visited grid x y]
  (let [currentVal (get-in grid [x y])
        unvisited (filter #(not (some visited [%])) neighbors)
        increasing-unvisited (filter #(> (get-in grid %) currentVal) unvisited)]
    increasing-unvisited))

(defn get-neighbors [x y grid]
  (let [ns [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]]]
    (filter #(get-in grid %) ns)))

(defn find-max [visited depth x y grid]
  (let [ns (get-neighbors x y grid)
        filtered-neighbors (unvisited-increasing-neighbors ns visited grid x y)]
    (if (empty? filtered-neighbors)
      depth
      (apply max (map #(find-max (conj visited %) (inc depth) (first %) (last %) grid) filtered-neighbors)))))

(defn longest-increasing [grid]
  (let [depths (for [x (range 0 (count grid))
                     y (range 0 (count (first grid)))]
                 (find-max #{} 1 x y grid))]
    (apply max depths)))

(longest-increasing [[1 2 2 4]
                    [4 5 4 1]
                    [7 2 6 7]])
