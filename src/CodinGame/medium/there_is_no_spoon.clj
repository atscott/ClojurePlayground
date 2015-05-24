(ns Player
  (:gen-class))


(defn get-or-dot [lines x y]
  (nth (nth lines y []) x "."))

(defn try-get-right [lines startX startY]
  (let [remaining-row (drop startX (nth lines startY []))
        index (.indexOf remaining-row \0)]
    (if (> index -1) (str (+ index startX) " " startY) "-1 -1")))

(defn try-get-down [lines startX startY]
  (let [remaining-column (map #(nth % startX) (drop startY lines))
        index (.indexOf remaining-column \0)]
    (if (> index -1) (str startX " " (+ startY index)) "-1 -1")))


(defn print-result [self right bottom]
  (print self " ")
  (print right " ")
  (println bottom))

(defn -main [& args]
  (let [width (read) height (read) _ (read-line)
        lines (->> (repeatedly height read-line)
                   (doall)
                   (apply str)
                   (partition-all width))]
    (binding [*out* *err*]
      (println "lines: " lines " width: " width " height: " height))
    (doseq [x (range 0 width)]
      (doseq [y (range 0 height)]
        (let [self (get-or-dot lines x y)
              right (try-get-right lines (inc x) y)
              bottom (try-get-down lines x (inc y))]
          (if (= \0 self)
            (print-result (str x " " y) right bottom)
            (binding [*out* *err*]
              (println "skipping " x " " y))
            )
          )
        ))))
