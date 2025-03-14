;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L19_P2-misc-list-absts) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; SORT
; Another useful abstraction is sort:

; (X) [List-of X] [X X -> Boolean] -> [List-of X]
; produces a version of lx that is sorted according to cmp
#;
(define (sort lx cmp) ...)

; For example...

; mysort : [List-of Number] -> [List-of Number]
; Sorts a list of numbers
 
(check-expect (mysort '()) '())
(check-expect (mysort (list 9 2 5)) (list 2 5 9))
(check-expect (mysort (list 9 2 5 1 0 7)) (list 0 1 2 5 7 9))
 
(define (mysort lon)
  (sort lon <))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise #1:
; Design a function reverse-alpha to sort a list of strings reverse alphabetically.

; reverse-alpha : [List-of String] -> [List-of String]
; Sorts a list of string reverse-alphabetically
 
(check-expect (reverse-alpha '()) '())
(check-expect (reverse-alpha (list "c" "a" "t")) (list "t" "c" "a"))
(check-expect (reverse-alpha (list "l" "o" "o" "k")) (list "o" "o" "l" "k"))
 
(define (reverse-alpha los)
  (sort los string>?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise #2:
; Design a function num-size to sort a list of numberrs by number of digits

; num-size : [List-of Nat] -> [List-of Nat]
; Sorts a list of numbers by number of digits
 
(check-expect (num-size '()) '())
(check-expect (num-size (list 8675 309)) (list 309 8675))
(check-expect (num-size (list 1 23 456 789)) (list 1 23 456 789))
 
(define (num-size lon)
  ;<DO NOW><SCROLL>







  
  (map string->number
       (sort (map number->string lon) len<?)))

; len<? : String String -> Boolean
; does the first string have fewer characters?

(check-expect (len<? "a" "bc") #true)
(check-expect (len<? "cd" "e") #false)

(define (len<? s1 s2)
  (< (string-length s1)
     (string-length s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BUILD-LIST
; And last, but not least... build-list:

; (X) Nat [Nat -> X] -> [List-of X]
; Applies f to the first n natural numbers
; (build-list n f) == (list (f 0) (f 1) (f 2) ... (f (sub1 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Use build-list to design a function which computes the first n double-squares,
; or numbers that are twice as large as perfect squares.

; double-squares : Nat -> [List-of Nat]
; The first n double squares

(check-expect (double-squares 0) '())
(check-expect (double-squares 4) (list 0 2 8 18))
 
(define (double-squares n)
  ;<DO NOW><SCROLL>







  
  (build-list n double-square))

; double-square : Nat -> Nat
; The nth double square

(check-expect (double-square 0) 0)
(check-expect (double-square 1) 2)
(check-expect (double-square 2) 8)
(check-expect (double-square 3) 18)

(define (double-square n)
  (* 2 (sqr n)))
