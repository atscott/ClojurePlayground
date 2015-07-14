(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [n (read) _ (read-line) vs (repeatedly n #(read))
        extremes (reduce (fn [m x]
                           (cond
                             (< x (:max m)) {:drop (min (- x (:max m)) (:drop m)) :max (:max m)}
                             (> x (:max m)) {:drop (:drop m) :max x}
                             :else m))
                         {:drop 0 :max (first vs)} (rest vs))]
    (println (:drop extremes))))