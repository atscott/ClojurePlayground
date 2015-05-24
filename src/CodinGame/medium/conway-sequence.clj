(ns Solution
  (:gen-class))

(defn conway [R L]
  (loop [previous-line [R] i 1]
    (if (= i L)
      (->> (interpose " " previous-line)
           (apply str)
           (println))
      (let [next-line (->> (partition-by identity previous-line)
                           (mapcat #(vector (count %) (first %))))]
        (recur next-line (inc i))))))

(defn -main [& args]
  (conway (read) (read)))

