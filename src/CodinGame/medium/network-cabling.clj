(ns Solution
  (:gen-class))

(defn median [xs]
  (let [ns (sort xs)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn diff [x y] (Math/abs (- x y)))

(defn list-range [xs]
  (let [ns (sort xs)]
    (diff (last ns) (first ns))))


(defn -main [& args]
  (let [N (read)
        coords (repeatedly N #(vector (read) (read)))
        ys (map second coords)
        mid (int (median ys))
        yLength (reduce #(+ % (diff mid %2)) 0 ys)
        xLength (list-range (map first coords))]

    (println (+ xLength yLength))))
