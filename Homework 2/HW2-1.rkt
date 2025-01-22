#lang htdp/bsl

;; Exercise 1
;; divides? : Number Number -> Boolean
;; Determine if the numbers divide each other evenly
(check-expect (divides? 4 2) #t)
(check-expect (divides? 4 3) #f)
(check-expect (divides? 2 4) #t)

(define (divides? a b)
  (cond 
    [(= (remainder a b) 0) #t]
    [(= (remainder b a) 0) #t]
    [else #f]))
