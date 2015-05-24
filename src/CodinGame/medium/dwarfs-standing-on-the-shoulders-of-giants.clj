(ns Solution
  (:gen-class))

(defn depth [root tree]
  (let [children (tree root)
        child-depths (map #(inc (depth % tree)) children)]
    (if (nil? children)
      1
      (apply max child-depths))))

(defn depth-tail-rec [root tree]
  (loop [roots #{root} d 0]
    (if (empty? roots)
      d
      (let [children (reduce #(apply hash-set (concat % (tree %2))) #{} roots)]
        (recur children (inc d))))))

(defn read-influences [n]
  (let [pairs (doall (repeatedly n #(vector (read) (read))))]
    (reduce (fn [m [x y]] (assoc m x (concat [y] (m x)))) {} pairs)))

(defn -main [& args]
  (let [n (read)
        influences (read-influences n)
        max-depth (reduce #(max % (depth-tail-rec %2 influences)) 0 (keys influences))]
    (println max-depth)))
