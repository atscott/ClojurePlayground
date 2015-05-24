(ns Solution
  (:gen-class))

(defn get-result [strengths]
  (->> (sort strengths)
       (#(map list % (rest %)))
       (reduce #(min % (Math/abs (- (first %2) (second %2)))) Integer/MAX_VALUE)))

(defn -main [& args]
  (let [strengths (repeatedly (read) read)]
    (prn (get-result strengths))))

