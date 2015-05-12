(ns Player
  (:gen-class))

(defn read-elevators [n]
  (into {} (doall (repeatedly n #(let [a (read) b (read)] [a b])))))

(defn wait-if-correct-direction [pos targetPos direction]
  (cond
    (and (<= pos targetPos) (= direction "RIGHT")) (println "WAIT")
    (and (>= pos targetPos) (= direction "LEFT")) (println "WAIT")
    :else (println "BLOCK")))



(defn -main [& args]
  (let [[nbFloors width nbRounds exitFloor exitPos nbTotalClones nbAdditionalElevators nbElevators] (repeatedly 8 read)
        elevators (read-elevators nbElevators)]
    (letfn [(something [newElevators]
                       (let [cloneFloor (read) clonePos (read) direction (str (read))
                             elevatorPosOnFloor ((conj elevators newElevators) cloneFloor)]

                         (cond
                           (= -1 clonePos) (do (println "WAIT") (something newElevators))
                           (= exitFloor cloneFloor) (do (wait-if-correct-direction clonePos exitPos direction) (something newElevators))
                           (nil? elevatorPosOnFloor) (do (println "ELEVATOR") (something (conj newElevators {cloneFloor clonePos})))
                           :else (do (wait-if-correct-direction clonePos elevatorPosOnFloor direction) (something newElevators)))))]
      (something {}))))
