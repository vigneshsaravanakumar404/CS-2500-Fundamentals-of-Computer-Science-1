;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L18_P2-collapse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; COLLAPSE

; One more exercise in designing a list abstraction:

; A [List-of Number] is one of
; - '()
; - (cons Number [List-of Number])
; Interpretation: ...
 
(define LON-0 '())
(define LON-1 (cons 2 LON-0))
(define LON-2 (cons 3 LON-1))
(define LON-3 (cons 4 LON-2))
 
(define (lon-temp lon)
  (...
   (cond [(empty? lon) ...]
         [(cons? lon) (... (first lon) ...
                           (lon-temp (rest lon)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Group A:
;   Design find-sum that accepts a list of numbers and computes the sum.
;
; Group B:
;   Design find-product that accepts a list of numbers and computes the product.
 
; find-sum : [List-of Number] -> Number
; Finds the sum of the numbers in the list
 
(check-expect (find-sum LON-0) 0)
(check-expect (find-sum LON-3) 9)
 
(define (find-sum lon)
  ;<DO NOW>

  )
 
; find-product : [List-of Number] -> Number
; Finds the product of the numbers in the list
 
(check-expect (find-product LON-0) 1)
(check-expect (find-product LON-3) 24)
 
(define (find-product lon)
  ;<DO NOW>

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 1: What are the differences?
;<DISCUSS>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 2: Parameterize the differences
;   (Let's call this list abstraction: collapse

; collapse : ??? <DO NOW><SCROLL> e.g.: (collapse lon 0 +)
; Collapses a list given a base-case and pairwise function







; collapse : (X) [List-of X] X [X X -> X] -> X
 
(check-expect (collapse LON-0 0 +) 0)
(check-expect (collapse LON-3 0 +) 9)
(check-expect (collapse LON-0 1 *) 1)
(check-expect (collapse LON-3 1 *) 24)
 
(define (collapse lon base f)
  ;<DO NOW><SCROLL>







  
  (cond [(empty? lon) base]
        [(cons? lon) (f (first lon)
                        (collapse (rest lon) base f))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 3: Reimplement originals in terms of new abstraction

(define (find-sum lon)
  (collapse lon 0 +))
 
(define (find-product lon)
  (collapse lon 1 *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Are we fully utilizing the power of collapse ?
; Consider that we can actually write the following use of collapse

; total-length : [List-of String] -> Number
; Returns the total length of all strings
 
(check-expect (total-length '()) 0)
(check-expect (total-length (cons "hi" (cons "there" '()))) 7)
 
(define (total-length los)
  (collapse los 0 add-string-to-sum))
 
; add-string-to-sum : String Number -> Number
; Adds the length of the String to the passed-in number
 
(check-expect (add-string-to-sum "hi" 0) 2)
(check-expect (add-string-to-sum "there" 2) 7)
 
(define (add-string-to-sum str num)
  (+ (string-length str) num))

; What does this imply about our signature?
; It was too restrictive - so our final signature...

; collapse : (X Y) [List-of X] Y [X Y -> Y] -> Y


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIST
;
; Even though though BSL/ISL lists ('()/cons) are more convenient than custom self-
; referential collections, it can get tiresome to constantly write (cons a (cons b ... '())).
; There is a convenient shorthand that ISL provides for you: list.

; Vocabulary	list
; Grammar	(list value value ...) where value is any value
; Semantics	list is simply a way to save yourself from writing cons over and over--
;		what it creates is still a normal cons list

; (list 1 2 3 4)
; =
; (cons 1 (cons 2 (cons 3 (cons 4 '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; For the following lists, create them using only the list function:
;<DO NOW>

; (cons 1 '())

; (cons 3 (cons "hello" (cons (make-posn 0 8) '())))

; '()

; (cons (cons 1 (cons 2 '())) (cons 3 '()))

; (cons "a" (list 0 #false))

; What does this do?
(define MYLIST1 (list 1
                      (list 2 3)))

; NB: the list function will *always* create a list with exactly as many
; items as you arguments you called it with.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complication: Treating functions as values

;; Try cutting-and-pasting following into new window
;
;; Try 1:
;(check-expect (choose-func #true) +)
;(check-expect (choose-func #false) -)
;
;; Try 2:
;(check-expect ((choose-func #true) 1 1) 2)
;(check-expect ((choose-func #false) 1 1) 0)
;
;; Try 3:
;(define F-TRUE (choose-func #true))
;(define F-FALSE (choose-func #false))
;(check-expect (F-TRUE 1 1) 2)
;(check-expect (F-FALSE 1 1) 0)
;
(define (choose-func x)
  (if x + -))
;
;; Why the error even on Try 3?
;
;; Unfortunately, this also implies the check-expects have to come
;; *after* the code for the function.
;; (Why now, but not before? <EXPLAIN>)
;
;; This will have to do... for now.
