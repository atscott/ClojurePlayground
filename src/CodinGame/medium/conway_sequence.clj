(ns Solution
  (:gen-class))

(use '[clojure.test :as t])

(defn conway
  {:test (fn []
           (is (= [1 1 1 3 1 2 2 1 1 3 3 1 1 2 1 3 2 1 1 3 2 1 2 2 2 1] (conway 1 11)))
           (is (= [2] (conway 2 1)))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 5] (conway 5 10)))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 25] (conway 25 10))))}
  [R L]
  (loop [previous-line [R] i 1]
    (if (= i L)
      (do (->> (interpose " " previous-line)
               (apply str)
               (println))
          previous-line)
      (let [next-line (->> (partition-by identity previous-line)
                           (mapcat #(vector (count %) (first %))))]
        (recur next-line (inc i))))))

(defn conway-2
  {:test (fn []
           (is (= [1 1 1 3 1 2 2 1 1 3 3 1 1 2 1 3 2 1 1 3 2 1 2 2 2 1] (nth (conway-2 1) 11)))
           (is (= [2] (first (conway-2 2))))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 5] (nth (conway-2 5) 10)))
           (is (= [3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2 3 1 1 3 3 2 2 1 1 25] (nth (conway-2 25) 10))))}
  [seed]
  (cons [seed]
        (iterate #(->> (partition-by identity %)
                       (mapcat (juxt count first)))
                 [seed])))

(defn -main [& args]
  (let [start (read) line (read)]
    (->> (conway-2 start)
         (#(nth % line))
         (interpose " ")
         (println))))

;(t/run-tests)