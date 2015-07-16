(ns Solution
  (:gen-class))

(use '[clojure.test :as t])

(defn conway
  "only returns the requested line for the given seed"
  {:test (fn []
           (is (= [1 1 1 3 1 2 2 1 1 3 3 1 1 2 1 3 2 1 1 3 2 1 2 2 2 1] (conway 1 11)))
           (is (= [2] (conway 2 1)))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 5] (conway 5 10)))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 25] (conway 25 10))))}
  [seed line]
  (loop [previous-line [seed] i 1]
    (if (= i line)
      (do (->> (interpose " " previous-line)
               (apply str)
               (println))
          previous-line)
      (let [next-line (->> (partition-by identity previous-line)
                           (mapcat #(vector (count %) (first %))))]
        (recur next-line (inc i))))))

(defn conway-2
  "returns lazy sequence"
  {:test (fn []
           (is (= [1 1 1 3 1 2 2 1 1 3 3 1 1 2 1 3 2 1 1 3 2 1 2 2 2 1] (nth (conway-2 1) 10)))
           (is (= [2] (first (conway-2 2))))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 5] (nth (conway-2 5) 9)))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 25] (nth (conway-2 25) 9))))}
  [seed]
  (iterate #(->> (partition-by identity %)
                 (mapcat (juxt count first)))
           [seed]))

(defn -main [& args]
  (let [start (read) line (read)]
    (->> (conway-2 start)
         (#(nth % (dec line)))
         (interpose " ")
         (println))))

(t/run-tests)
