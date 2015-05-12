(ns Player
  (:gen-class))

(defn read-elevators [n]
  (into {} (doall (repeatedly n #(let [a (read) b (read)] [a b])))))

(defn correct-direction [pos targetPos direction]
  (cond
    (and (<= pos targetPos) (= direction "RIGHT")) true
    (and (>= pos targetPos) (= direction "LEFT")) true
    :else false))

(defn -main [& args]
  (let [[nbFloors width nbRounds exitFloor exitPos nbTotalClones nbAdditionalElevators nbElevators] (doall (repeatedly 8 read))
        elevators (read-elevators nbElevators)]
    (while true
      (let [cloneFloor (read) clonePos (read) direction (str (read))
            elevatorPosOnFloor (elevators cloneFloor)]

        (binding [*out* *err*] (println "cloneFloor" cloneFloor "clonePos" clonePos "direction" direction "elevatorPosOnFloor" elevatorPosOnFloor))

        (cond
          (= -1 clonePos) (println "WAIT")
          (= exitFloor cloneFloor) (if (correct-direction clonePos exitPos direction) (println "WAIT") (println "BLOCK"))
          :else (if (correct-direction clonePos elevatorPosOnFloor direction) (println "WAIT") (println "BLOCK")) )))))
