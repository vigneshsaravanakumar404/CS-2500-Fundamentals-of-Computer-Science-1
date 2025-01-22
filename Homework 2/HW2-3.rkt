#lang htdp/bsl

;; ====================================================
;; Exercise 3

;; next-collatz : Number -> Number
;; Given a number n, returns the next number in the Collatz sequence.
;; Example: (next-collatz 5) => 16
(define (next-collatz n)
  (cond 
    [(= (remainder n 2) 0) (/ n 2)]
    [else (+ (* 3 n) 1)]))

;; Tests
;; (next-collatz 27) ; should be 82
;; (next-collatz 82) ; should be 41
;; (next-collatz 2) ; should be 1
;; ====================================================