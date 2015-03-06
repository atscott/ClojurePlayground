;https://projecteuler.net/problem=1
(ns euler-1)

(defn is_div_3or5? [x]
  (or
    (zero? (mod x 3))
    (zero? (mod x 5))
    )
  )

(reduce + (filter is_div_3or5? (range 1000)))

(reduce + (set (concat (range 0 1000 5) (range 0 1000 3))))
