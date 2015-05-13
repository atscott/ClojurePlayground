(ns Solution
  (:gen-class))

(defn depth [root tree]
  (let [children (tree root)
        child-depths (map #(inc (depth % tree)) children)]
    (if (nil? children)
      1
      (apply max child-depths))))

(defn read-influences [n]
  (let [pairs (doall (repeatedly n #(let [x (read) y (read)] [x y])))]
    (reduce (fn [m [x y]] (assoc m x (concat [y] (m x)))) {} pairs)))

(defn -main [& args]
  (let [n (read)
        influences (read-influences n)
        memo-depth (memoize depth)
        max-depth (reduce #(max % (memo-depth %2 influences)) 0 (keys influences))]

    (println max-depth)))