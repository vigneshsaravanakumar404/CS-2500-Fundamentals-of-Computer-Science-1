;; ====================================================
;; Exercise 1
;; divides? : Number Number -> Boolean
;; Determine if the numbers divide each other evenly
;; Example: (divides? 4 2) => #t
(define (divides? a b)
  (cond 
    [(= (remainder a b) 0) #t]
    [(= (remainder b a) 0) #t]
    [else #f]))

;; Tests
;; (divides? 4 2) ; should be #t
;; (divides? 4 3) ; should be #f
;; (divides? 2 4) ; should be #f
;; ====================================================
