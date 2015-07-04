(ns Player
  (:gen-class))

(use '[clojure.test :as t])

(def num-stones 30)

(defn to-stone
  {:test (fn []
           (is (= 0 ((to-stone 0 0) :dist)))
           (is (= {:dist 1 :dir \>} (to-stone 0 1)))
           (is (= {:dist 1 :dir \<} (to-stone 0 (dec num-stones)))))}
  [from to]
  (let [d (Math/abs (int (- from to)))]
    (cond
      (and (> d 15) (< from to)) {:dir \< :dist (- num-stones d)}
      (and (> d 15) (> from to)) {:dir \> :dist (- num-stones d)}
      (< from to) {:dir \> :dist d}
      :else {:dir \< :dist d})))

(defn to-letter
  {:test (fn []
           (is (= 0 ((to-letter \A \A) :dist)))
           (is (= {:dist 1 :dir \+}) (to-letter \A \B))
           (is (= {:dist 2 :dir \-}) (to-letter \A \Z)))}
  [from to]
  (let [from-int (if (= \space from) (inc (int \Z)) (int from))
        to-int (if (= \space to) (inc (int \Z)) (int to))
        d (Math/abs (- (int from-int) (int to-int)))]
    (cond
      (and (> d 13) (< from-int to-int)) {:dir \- :dist (inc (- 26 d))}
      (and (> d 13) (> from-int to-int)) {:dir \+ :dist (inc (- 26 d))}
      (< from-int to-int) {:dir \+ :dist d}
      :else {:dir \- :dist d})))

(defn closest-stone-to-letter [letter stones position]
  ((reduce
     #(let [d (+ ((to-letter (nth stones %2) letter) :dist)
                 ((to-stone position %2) :dist))]
       (if (< d (% :dist))
         {:pos %2 :dist d}
         %))
     {:pos 0 :dist Integer/MAX_VALUE}
     (range 30))
    :pos))

(defn next-letter
  {:test (fn []
           (is (= \B (next-letter \+ \A)))
           (is (= \A (next-letter \- \B)))
           (is (= \A (next-letter \+ \space)))
           (is (= \space (next-letter \+ \Z)))
           (is (= \space (next-letter \- \A)))
           (is (= \Z (next-letter \- \space))))}
  [command current-letter]
  (let [ascii (if (= \+ command)
                (inc (int current-letter))
                (dec (int current-letter)))]
    (cond
      (= (inc (int \Z)) ascii) \space
      (= (dec (int \A)) ascii) \space
      (= (inc (int \space)) ascii) \A
      (= (dec (int \space)) ascii) \Z
      :else (char ascii))))

(defn get-next-move
  {:test (fn []
           (is (= \. (get-next-move "" "A" (repeat num-stones \A) 0)))
           (is (= \+ (get-next-move "" "B" (repeat num-stones \A) 0)))
           (is (= \- (get-next-move "" "A" (repeat num-stones \B) 0)))
           (is (= \> (get-next-move "" "A" (-> (repeat num-stones \C) vec (assoc 1 \A)) 0)))
           (is (= \< (get-next-move "" "A" (-> (repeat num-stones \C) vec (assoc 0 \A)) 1))))}
  [current-phrase magic-phrase stones position]
  (let [target-letter (first (drop (count current-phrase) magic-phrase))
        target-stone (closest-stone-to-letter target-letter stones position)
        letter-on-stone (nth stones target-stone)]
    (cond
      (and (= position target-stone) (= letter-on-stone target-letter)) \.
      (= position target-stone) ((to-letter letter-on-stone target-letter) :dir)
      :else ((to-stone position target-stone) :dir))))

(defn do-problem
  {:test (fn []
           (is (= "+.--." (do-problem "AZ"))))}
  [magicPhrase]
  (loop [current-phrase "" stones (vec (repeat num-stones \space)) position 0 moves []]
    (if (= current-phrase magicPhrase)
      (do (println (apply str moves)) (apply str moves))
      (let [n-move (get-next-move current-phrase magicPhrase stones position)
            n-phrase (if (= \. n-move) (str current-phrase (nth stones position)) current-phrase)
            n-pos (cond
                    (and (= \> n-move) (= (dec num-stones) position)) 0
                    (and (= \< n-move) (= 0 position)) (dec num-stones)
                    (= \> n-move) (inc position)
                    (= \< n-move) (dec position)
                    :else position)
            n-stones (if (or (= \+ n-move) (= \- n-move))
                       (assoc stones position (next-letter n-move (nth stones position)))
                       stones)]
        (recur n-phrase n-stones n-pos (conj moves n-move))))))

(defn -main [& args]
  (let [magicPhrase (read-line)]
    (do-problem magicPhrase)))

(t/run-tests)
