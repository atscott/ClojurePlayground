(ns Solution
  (:gen-class))

(defn build-adjacency-list [m [p1 p2]]
  (-> (assoc m p1 (conj (m p1) p2))
      (assoc p2 (conj (m p2) p1))))

(def depth-memo #(0))
(defn depth [start parent adjacency-list]
  (let [neighbors (filter #(not (= parent %)) (adjacency-list start))]
    (if (empty? neighbors)
      0
      (apply max (map #(inc (depth-memo % start adjacency-list)) neighbors)))))
(def depth-memo (memoize depth))

(defn -main [& args]
  (let [n (read)
        pairs (repeatedly n #(vector (read) (read)))
        adjacency-list (reduce #(build-adjacency-list % %2) {} pairs)]

    (println (reduce #(min % (depth-memo %2 nil adjacency-list)) Integer/MAX_VALUE (keys adjacency-list)))))
