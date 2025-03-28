;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; TwoList is (list Any Any)
; Interpretation: a list of two elements
(define EX-TWOLIST-1 (list 1 2))
(define EX-TWOLIST-2 (list 3 4))
(define EX-TWOLIST-3 (list 5 6))

(define (twolist-temp twolist)
  (... t1 ...))

; Exercise 2a
; zip : [List-of Any] [List-of Any] -> TwoList
; to produce a list of lists where each sublist contains the ith element of the first list 
; and the ith element of the second list
(check-expect (zip (list 1 3 5 7) (list 2 4 6 8)) (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
(check-expect (zip (list 1 3 5 7) (list 2 4 6)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list 1 3 5) (list 2 4 6 8)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list 1) (list 2 4 6 8)) (list (list 1 2)))
(check-expect (zip '() (list 2 4 6 8)) '())
(check-expect (zip (list 1 3 5 7) '()) '())
(check-expect (zip '() '()) '())

(define (zip l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2)))]))

; Exercise 2b
; map-2list : (X Y -> Z) [List-of X] [List-of Y] -> [List-of Z]
; to produce a list of the results of applying the function f to the ith element of the first list
(check-expect (map-2list + (list 1 2 3) (list 4 5 6)) (list 5 7 9))
(check-expect (map-2list * (list 1 2 3) (list 4 5 6)) (list 4 10 18))
(check-expect (map-2list - (list 1 2 3) (list 4 5 6)) (list -3 -3 -3))
(check-expect (map-2list string-append (list "a" "b" "c") (list "d" "e" "f")) (list "ad" "be" "cf"))
(check-error (map-2list + (list 1 2 3) (list 4 5)) "Lists of unequal size")
(check-error (map-2list + (list 1 2) (list 4 5 6)) "Lists of unequal size")

(define (map-2list f l1 l2)
  (cond
    [(not (= (length l1) (length l2))) (error "Lists of unequal size")]
    [(empty? l1) empty]
    [else (cons (f (first l1) (first l2)) (map-2list f (rest l1) (rest l2)))]))
