;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L17_P4-ISL-list-abst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List abstractions are very useful, so ISL provides pre-defined versions of
; the more useful ones.
; Refer to Figs. 95 and 96 in the textbook
;   https://htdp.org/2021-5-4/Book/part_three.html#%28part._ch~3a3use%29

; Let's start with one that we’ve already seen: do-to-all.
; In ISL, this is called map:

; (X Y) [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx
; (map f (list x-1 ... x-n)) == (list (f x-1) ... (f x-n))

(define (map f lx) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function add-7-to-all that accepts a [List-of Number] and adds 7 to every number.
; add7-to-all : [List-of Number] -> [List-of Number]
; Adds 7 to every number

(check-expect (add7-to-all '()) '())
(check-expect (add7-to-all (list -1 0 3)) (list 6 7 10))
 
(define (add7-to-all lon)
  (map ...))
;<DO NOW> Helper: add7







; add7 : Number -> Number
; Adds 7 to the input

(check-expect (add7 -2.5) 4.5)
(check-expect (add7 0) 7)
(check-expect (add7 3) 10)
 
(define (add7 n)
  (+ 7 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The next one is keep-if

; the ISL-provided version is called filter:

; (X) [X -> Boolean] [List-of X] -> [List-of X]
; produces a list from those items on lx for which p holds 
(define (filter p lx) ...)

; Again, it's precisely the same as the keep-if function that we implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function only-negatives that accepts a [List-of Number] and
; keeps only the negative numbers.

; only-negatives : [List-of Number] -> [List-of Number]
; Keeps only the negative numbers

(check-expect (only-negatives '()) '())
(check-expect (only-negatives (list 8 6 7 5 3 0 9)) '())
(check-expect (only-negatives (list 8 -6 7 -5 3 0 -9)) (list -6 -5 -9))
 
(define (only-negatives lon)
  (filter negative? lon))
