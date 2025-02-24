;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW7-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; until-something : (X) [X X -> Boolean] X -> [List-of X]
; Interpretation: To return a list of elements from a list until a certain condition is met.
(check-expect (until-something = 0 (list 1 2 3 0 4 5)) (list 1 2 3))
(check-expect (until-something = 0 empty) empty)
(check-expect (until-something = 0 (list 0)) empty)
(check-expect (until-something = 0 (list 1 2 3 4 5)) (list 1 2 3 4 5))
(check-expect (until-something string=? "." (list "Hello" "World" "." "Goodbye")) (list "Hello" "World"))
(check-expect (until-something string=? "." empty) empty)
(check-expect (until-something string=? "." (list ".")) empty)
(check-expect (until-something string=? "." (list "This" "is" "a" "test")) (list "This" "is" "a" "test"))

(define (until-something f s loX)
  (cond
    [(empty? loX) empty]
    [(cons? loX) (if (f (first loX) s)
                     empty
                     (cons (first loX) (until-something f s (rest loX))))]))



; until-zero: [List-of Number] -> [List-of Number]
; Interpretation To return a list of numbers from a list of numbers until a zero is reached.
(check-expect (until-zero (list 1 2 3 4 5)) (list 1 2 3 4 5))
(check-expect (until-zero (list 1 2 3 0 4 5)) (list 1 2 3))
(check-expect (until-zero (list 0)) empty)
(check-expect (until-zero empty) empty)

(define (until-zero lon)
    (until-something = 0 lon))
 

; words-until-period: [List-of String] -> [List-of String]
; Interpretation To return a list of words from a list of words until a period is reached.
(check-expect (words-until-period (list "This" "is" "a" "test")) (list "This" "is" "a" "test"))
(check-expect (words-until-period (list "Hello" "World" "." "Goodbye")) (list "Hello" "World"))
(check-expect (words-until-period (list ".")) empty)
(check-expect (words-until-period empty) empty)

(define (words-until-period los)
    (until-something string=? "." los))

