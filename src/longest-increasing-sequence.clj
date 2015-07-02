(defn get-neighbors [x y grid]
  (let [ns [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]]]
    (filter #(get-in grid %) ns)))

(declare memo-increasing)
(defn increasing [x y grid]
  (let [ns (get-neighbors x y grid)
        increasing-neighbors (filter #(> (get-in grid %) (get-in grid [x y])) ns)]
    (if (empty? increasing-neighbors)
      1
      (apply max (map (fn [[nx ny]] (inc (memo-increasing nx ny grid))) increasing-neighbors)))))
(def memo-increasing (memoize increasing))

(defn find-increasing [grid]
  (apply max (for [x (range 0 (count grid))
                   y (range 0 (count (first grid)))]
               (memo-increasing x y grid))))

(find-increasing [[14 13 21 32 38]
                  [95 14 55 88 88]
                  [12 32 56 17 14]
                  [19 90 10 77 58]
                  [44 98 19 54 16]])

(find-increasing [[1 2 8 4]
                  [5 6 8 8]
                  [9 1 4 3]])

(let [million-matrix (into [] (repeatedly 1000 #(into [] (repeatedly 1000 (fn [] (rand-int 100))))))]
  (find-increasing million-matrix))