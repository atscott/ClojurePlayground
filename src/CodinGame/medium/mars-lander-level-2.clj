(ns Player
  (:gen-class))


(defn -main [& args]
  (let [N (read)
        landCoords (doall (repeatedly N #(vector (read) (read))))
        ;landingCoordinates
        [[lX1 lY1] [lX2 lY2]] (->> (partition-by second landCoords)
                                   (some #(if (= (count %) 2) %))
                                   (into []))]
    (while true
      (let [X (read) Y (read) HS (read) VS (read) F (read) R (read) P (read)
            desired-angle (cond
                            (< (- Y lY1) 100) 0
                            (or (< VS -40) (and (< HS -20) (> HS -60) (> X lX2))) 0
                            (and (< X lX2) (> X lX2) (< -30 HS)) -45
                            (and (< X lX2) (> X lX1) (> HS 30)) 45
                            (and (< VS -10) (> HS 20)) 45
                            (and (< VS -10) (< HS -20)) -45
                            (and (< VS -10) (> X lX2)) 45
                            (< X lX1) -45
                            (and (> X lX2) (< -60 HS)) 45
                            (and (> (- Y lY1) 100) (> HS 10)) 45
                            (and (> 100 (- Y lY1)) (> HS 10)) 45
                            (> HS 20) 45
                            (< HS -20) -45
                            :else 0)]

        (cond
          (or (< R 45) (> R -45)) (println desired-angle 4)
          (< 90 (Math/abs (- desired-angle R))) (println desired-angle 0)
          :else (println desired-angle 4))
        ; HS: the horizontal speed (in m/s), can be negative.
        ; VS: the vertical speed (in m/s), can be negative.
        ; F: the quantity of remaining fuel in liters.
        ; R: the rotation angle in degrees (-90 to 90).
        ; P: the thrust power (0 to 4).

        ; R P. R is the desired rotation angle. P is the desired thrust power.
        ))))