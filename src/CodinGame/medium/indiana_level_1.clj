(ns Player
  (:gen-class))

(defn get-result-direction [type entering-direction]
  (let [type-map {1  {"TOP" "BOTTOM", "LEFT" "BOTTOM", "RIGHT" "BOTTOM"}
                  2  {"LEFT" "RIGHT", "RIGHT" "LEFT"}
                  3  {"TOP" "BOTTOM"}
                  4  {"TOP" "LEFT", "RIGHT" "BOTTOM"}
                  5  {"LEFT" "BOTTOM", "TOP" "RIGHT"}
                  6  {"LEFT" "RIGHT", "RIGHT" "LEFT"}
                  7  {"TOP" "BOTTOM", "RIGHT" "BOTTOM"}
                  8  {"LEFT" "BOTTOM", "RIGHT" "BOTTOM"}
                  9  {"TOP" "BOTTOM", "LEFT" "BOTTOM"}
                  10 {"TOP" "LEFT"}
                  11 {"TOP" "RIGHT"}
                  12 {"RIGHT" "BOTTOM"}
                  13 {"LEFT" "BOTTOM"}}]
    ((type-map type) (str entering-direction))))

(defn get-next-coords [iX iY direction]
  (cond
    (= direction "BOTTOM") [iX (inc iY)]
    (= direction "TOP") [iX (dec iY)]
    (= direction "RIGHT") [(inc iX) iY]
    :else [(dec iX) iY]))

(defn read-line-types [width]
  (let [types (doall (repeatedly width read))
        _ (read-line)]
    types))

(defn -main [& args]
  (let [[width height] (repeatedly 2 read) _ (read-line)
        lines (doall (repeatedly height #(read-line-types width)))
        exit (read)]
    (while true
      (let [[x y entry-point] (repeatedly 3 read) _ (read-line)
            type (nth (nth lines y) x)
            result-direction (get-result-direction type entry-point)
            next-coords (get-next-coords x y result-direction)]

        (binding [*out* *err*]
          (println "type:" type "entry-point:" entry-point "result-dir:" (get-result-direction type entry-point)))

        (println (first next-coords) (last next-coords))))))