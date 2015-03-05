;; gorilla-repl.fileformat = 1

;; **
;;; # https://projecteuler.net/problem=1
;;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;; 
;;; Find the sum of all the multiples of 3 or 5 below 1000.
;; **

;; @@
(defn is_div_3or5? [x]
  (or
    (zero? (mod x 3))
    (zero? (mod x 5))
    )
  )

(reduce + (filter is_div_3or5? (range 1000)))
;; @@
;; =>
;;; 233168
;; <=

;; @@
(reduce + (set (concat (range 0 1000 5) (range 0 1000 3))))
;; @@
;; =>
;;; 233168
;; <=
