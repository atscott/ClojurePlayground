(ns engres)

(use '[clojure.test :as t])

(defn secret-function
  {:test (fn []
           (is (= [1] (first (secret-function))))
           (is (= [[1]
                   [1 1]
                   [1 2 1]
                   [1 3 3 1]
                   [1 4 6 4 1]] (take 5 (secret-function))))
           (is (= [1 10 45 120 210 252 210 120 45 10 1] (nth (secret-function) 10))))}
  []
  (let [zip-with (fn [f & colls]
                   (->> (apply interleave colls)
                        (partition (count colls))
                        (map #(apply f %))))]
  (iterate #(concat [1]
                    ;(map + % (rest %))
                    (zip-with + % (rest %))
                    [1])
           '(1))))

(t/run-tests)

