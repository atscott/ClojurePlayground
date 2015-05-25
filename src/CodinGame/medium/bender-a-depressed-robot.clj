(ns Solution
  (:gen-class))

(def priorities (atom ["SOUTH" "EAST" "NORTH" "WEST"]))
(def breaker-mode (atom false))
(def board (atom [[0]]))
(def done (atom false))
(def moves (atom []))
(def currentPos (atom [0 0]))
(def currentDir (atom "SOUTH"))
(def teleporters (atom []))
(def turn (atom 0))

(defn get-coords-for [board item]
  (for [x (range 0 (count board))
        y (range 0 (count (first board)))
        :let [c (get-in board [x y])]
        :when (= c item)]
    [x y]))

(defn dir-to-target-coords [dir]
  (cond
    (= dir "SOUTH") (assoc @currentPos 0 (inc (first @currentPos)))
    (= dir "NORTH") (assoc @currentPos 0 (dec (first @currentPos)))
    (= dir "EAST") (assoc @currentPos 1 (inc (second @currentPos)))
    (= dir "WEST") (assoc @currentPos 1 (dec (second @currentPos)))
    ))

(defn can-move-to-space [coord]
  (let [item-in-space (get-in @board coord)]
    (cond
      (not (or (= \X item-in-space) (= \# item-in-space))) true
      (and (= \X item-in-space) (= true @breaker-mode)) true
      :else false)))

(defn possible-direction-map []
  {"SOUTH" (can-move-to-space (dir-to-target-coords "SOUTH")),
   "NORTH" (can-move-to-space (dir-to-target-coords "NORTH")),
   "EAST"  (can-move-to-space (dir-to-target-coords "EAST")),
   "WEST"  (can-move-to-space (dir-to-target-coords "WEST"))})

(defn get-next-move []
  (let [board-space (get-in @board @currentPos)]
    (cond
      (= \N board-space) (swap! currentDir (fn [_] "NORTH"))
      (= \S board-space) (swap! currentDir (fn [_] "SOUTH"))
      (= \E board-space) (swap! currentDir (fn [_] "EAST"))
      (= \W board-space) (swap! currentDir (fn [_] "WEST"))
      (= \B board-space) (swap! breaker-mode not)
      (= \X board-space) (swap! board assoc-in @currentPos " ")
      (= \I board-space) (swap! priorities reverse)
      (= \$ board-space) (swap! done not)
      (= \T board-space) (swap! currentPos (fn [_] (first (remove #(= @currentPos %) @teleporters))))
      ))
  (binding [*out* *err*]
    (println "turn:" @turn)
    (doseq [l (assoc-in @board @currentPos "!")] (println l)))
  (if (not @done)
    (let [final-dir (some #(if ((possible-direction-map) %) %) (cons @currentDir @priorities))]
      (swap! currentPos (fn [_] (dir-to-target-coords final-dir)))
      (swap! currentDir (fn [_] final-dir))
      final-dir
      )))

(defn -main [& args]
  (let [L (read) C (read) _ (read-line)
        b (->> (repeatedly L read-line)
               (doall)
               (apply str)
               (partition-all C)
               (map vec)
               (into []))
        start (first (get-coords-for b \@))
        ts (get-coords-for b \T)]

    (swap! board (fn [_] b))
    (swap! currentPos (fn [_] start))
    (swap! teleporters concat ts)

    (while (and (not @done) (> 1000 @turn)) (do (swap! turn inc) (swap! moves conj (get-next-move))))

    (if (= 1000 @turn)
      (println "LOOP")
      (doseq [m @moves] (if (not (nil? m)) (println m))))))
