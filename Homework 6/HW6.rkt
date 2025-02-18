;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;                             Exercise 1
;; ========================================================================
(define LIST-1 (cons 1 (cons 2 (cons 3 '()))))
(define LIST-2 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
(define LIST-3 (cons 43 (cons 70 (cons 12 '()))))


; keep-greater-than : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers greater or equal to the given number
(check-expect (keep-greater-than LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than LIST-3 50) (cons 70 '()))

(define (keep-greater-than lon num)
  (cond
    [(empty? lon) '()]
    [(>= (first lon) num) (cons (first lon) (keep-greater-than (rest lon) num))]
    [else (keep-greater-than (rest lon) num)]))


; keep-less-than : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers less than the given number
(check-expect (keep-less-than LIST-1 2) (cons 1 '()))
(check-expect (keep-less-than LIST-2 2) (cons 1 '()))
(check-expect (keep-less-than LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than lon num)
  (cond
    [(empty? lon) '()]
    [(< (first lon) num) (cons (first lon) (keep-less-than (rest lon) num))]
    [else (keep-less-than (rest lon) num)]))


; keep-select: [List-of Number] Number ??? -> [List-of Number]
; Produces a list of numbers that satisfy the given predicate
(check-expect (keep-select LIST-1 2 >=) (cons 2 (cons 3 '())))
(check-expect (keep-select LIST-2 2 >=) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-select LIST-3 50 >=) (cons 70 '()))

(define (keep-select lon num pred)
  (cond
    [(empty? lon) '()]
    [(pred (first lon) num) (cons (first lon) (keep-select (rest lon) num pred))]
    [else (keep-select (rest lon) num pred)]))


; New Functions with Abstracted Functions
(check-expect (keep-greater-than/v2 LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than/v2 LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than/v2 LIST-3 50) (cons 70 '()))

(define (keep-greater-than/v2 lon num)
  (keep-select lon num >=))

(check-expect (keep-less-than/v2 LIST-1 2) (cons 1 '()))
(check-expect (keep-less-than/v2 LIST-2 2) (cons 1 '()))
(check-expect (keep-less-than/v2 LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than/v2 lon num)
  (keep-select lon num <))


;;                           Exercise 2
;; ========================================================================
(define BOOL-LIST-1 (cons #true (cons #false (cons #true '()))))
(define BOOL-LIST-2 (cons #false (cons #false (cons #true (cons #true '())))))
(define BOOL-LIST-3 (cons #true (cons #true (cons #false '()))))


; nth-is-true? : [List-of Boolean] Number -> Boolean
; Produces true if the nth element of the list is true counting from 0
(check-expect (nth-is-true? BOOL-LIST-1 0) #true)
(check-expect (nth-is-true? BOOL-LIST-1 1) #false)
(check-expect (nth-is-true? BOOL-LIST-1 2) #true)
(check-expect (nth-is-true? BOOL-LIST-2 0) #false)
(check-expect (nth-is-true? BOOL-LIST-2 1) #false)

(define (nth-is-true? lob n)
  (cond
    [(empty? lob) #false]
    [(= n 0) (first lob)]
    [else (nth-is-true? (rest lob) (- n 1))]))


; first-true-helper : [List-of Boolean] Number -> Number
; Produces the index of the first true element in the list and -1 if there is no true element
(check-expect (first-true-helper BOOL-LIST-1 0) 0)
(check-expect (first-true-helper BOOL-LIST-2 0) 2)
(check-expect (first-true-helper BOOL-LIST-3 0) 0)
(check-expect (first-true-helper (cons #false '()) 0) -1)

(define (first-true-helper lob n)
  (cond
    [(empty? lob) -1]
    [(first lob) n]
    [else (first-true-helper (rest lob) (+ n 1))]))

; first-true : [List-of Boolean] -> Number
; Produces the index of the first true element in the list and -1 if there is no true element
(check-expect (first-true BOOL-LIST-1) 0)
(check-expect (first-true BOOL-LIST-2) 2)
(check-expect (first-true BOOL-LIST-3) 0)
(check-expect (first-true (cons #false '())) -1)
(check-expect (first-true '(#false #false)) -1)
(check-expect (first-true '(#false #false #true)) 2)
(check-expect (first-true '(#false #true #false #true)) 1)
(check-expect (first-true '(#false #false #false)) -1)
(check-expect (first-true '(#false #false #false #false #true)) 4)


(define (first-true lob)
  (first-true-helper lob 0))

#| Doing it without a Helper function
(define (first-true lob)
  (cond 
    [(empty? lob) -1]
    [(first lob) 0]
    [(= -1 (first-true (rest lob))) -1]
    [else (+ 1 (first-true (rest lob)))])) |#


; set-true : [List-of Boolean] Number -> [List-of Boolean]
; n-th item in the list (counting from 0) converted from #false to #true, error if the old value was not #false)
(check-expect (set-true BOOL-LIST-1 1) (cons #true (cons #true (cons #true '()))))
(check-expect (set-true BOOL-LIST-2 1) (cons #false (cons #true (cons #true (cons #true '())))))
(check-error (set-true BOOL-LIST-1 2) "The old value was not #false")
(check-error (set-true BOOL-LIST-2 2) "The old value was not #false")

(define (set-true lob n)
  (cond
    [(empty? lob) '()]
    [(= n 0) (if (first lob) (error "The old value was not #false") (cons #true (rest lob)))]
    [else (cons (first lob) (set-true (rest lob) (- n 1)))]))


; draw-map : [List-of Boolean] -> Image