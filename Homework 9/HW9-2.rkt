;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW9-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;! TwoList design recipe
; TwoList is one of:
; - '()
; - (cons (list X Y) TwoList)
; Interpretation a list of lists where each sublist contains the ith element of the first list
; and the ith element of the second list


; Exercise 2a
; zip : [List-of X] [List-of Y] -> TwoList
; to produce a list of lists where each sublist contains the ith element of the first list 
; and the ith element of the second list
(check-expect (zip (list 1 3 5 7) (list 2 4 6)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list 1 3 5) (list 2 4 6 8)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list 1 3 5 7) (list 2 4 6 8)) (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
(check-expect (zip '() (list 2 4 6 8)) '())
(check-expect (zip (list 1 3 5 7) '()) '())
(check-expect (zip '()'()) '())


(define (zip l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2)))]))


; Exercise 2b
; map-2list : [List-of X] [List-of Y] (X Y -> Z) -> [List-of Z]
; to produce a list of the results of applying the function f to the ith element of the first list
(check-expect (map-2list (list 1 2 3) (list 4 5 6) +) (list 5 7 9))
(check-expect (map-2list (list 1 2 3) (list 4 5 6) *) (list 4 10 18))
(check-expect (map-2list (list 1 2 3) (list 4 5 6) -) (list -3 -3 -3))


(define (map-2list l1 l2 f)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (f (first l1) (first l2)) (map-2list (rest l1) (rest l2) f))]))
