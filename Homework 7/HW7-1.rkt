#lang htdp/isl

; until-something : (X) [X X -> Boolean] [List of X] -> [List-of X]
; Interpretation: To return a list of elements from a list until a certain condition is met.
(check-expect (until-something 0 = (list 1 2 3 0 4 5)) (list 1 2 3))
(check-expect (until-something 0 = empty) empty)
(check-expect (until-something 0 = (list 0)) empty)
(check-expect (until-something 0 = (list 1 2 3 4 5)) (list 1 2 3 4 5))
(check-expect (until-something "." string=? (list "Hello" "World" "." "Goodbye"))
              (list "Hello" "World"))
(check-expect (until-something "." string=? empty) empty)
(check-expect (until-something "." string=? (list ".")) empty)
(check-expect (until-something "." string=? (list "This" "is" "a" "test"))
              (list "This" "is" "a" "test"))

(define (until-something s f loX)
  (cond
    [(empty? loX) empty]
    [(cons? loX) (if (f (first loX) s)
                     empty
                     (cons (first loX) (until-something s f (rest loX))))]))



; until-zero: [List-of Number] -> [List-of Number]
; Interpretation To return a list of numbers from a list of numbers until a zero is reached.
(check-expect (until-zero (list 1 2 3 4 5)) (list 1 2 3 4 5))
(check-expect (until-zero (list 1 2 3 0 4 5)) (list 1 2 3))
(check-expect (until-zero (list 0)) empty)
(check-expect (until-zero empty) empty)

(define (until-zero lon)
  (until-something 0 = lon))
 

; words-until-period: [List-of String] -> [List-of String]
; Interpretation To return a list of words from a list of words until a period is reached.
(check-expect (words-until-period (list "This" "is" "a" "test")) (list "This" "is" "a" "test"))
(check-expect (words-until-period (list "Hello" "World" "." "Goodbye")) (list "Hello" "World"))
(check-expect (words-until-period (list ".")) empty)
(check-expect (words-until-period empty) empty)

(define (words-until-period los)
  (until-something "." string=? los))