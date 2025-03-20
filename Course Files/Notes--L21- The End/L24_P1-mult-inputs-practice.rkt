;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname L24_P1-mult-inputs-practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; LECTURE 25: PRACTICE WITH ABSTRACTIONS, MULTIPLE COMPLEX INPUTS
; Purpose	To practice with abstractions, multiple complex inputs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TWO LISTS

; Exercise:
; Design the function prizes that takes two parallel lists (one of names,
; the other of prize amounts) and produces a list that announces who
; gets what.
; If we run out of prizes, the person gets 0.

; prizes : [List-of String] [List-of Number] -> [List-of String]
; Awards each person in the first list a prize amount from the second;
; if run out of prizes, person gets 0 :(
 
(check-expect (prizes (list "alice" "bob" "carol") (list 100 50 10))
              (list "alice gets $100"
                    "bob gets $50"
                    "carol gets $10"))
 
(check-expect (prizes (list "alice" "bob" "carol" "dan") (list 100 50))
              (list "alice gets $100"
                    "bob gets $50"
                    "carol gets $0"
                    "dan gets $0"))
 
(check-expect (prizes (list "alice" "bob") (list 100 50 10))
              (list "alice gets $100"
                    "bob gets $50"))

(define (prizes/v1 los lon)
  ;<DO NOW><SCROLL>







  
  (local [; award : String Number -> String
          ; Produces an award result
          (define (award name amt)
            (string-append name " gets $" (number->string amt)))]
    (cond [(and (empty? los) (empty? lon)) '()]
          [(and (empty? los) (cons? lon)) '()]
          [(and (cons? los) (empty? lon)) (cons (award (first los) 0)
                                                (prizes (rest los) lon))]
          [(and (cons? los) (cons? lon))  (cons (award (first los) (first lon))
                                                (prizes (rest los) (rest lon)))])))

; Simplify:
  ;<DO NOW><SCROLL>







  
(define (prizes los lon)
  (local [; award : String Number -> String
          ; Produces an award result
          (define (award name amt)
            (string-append name " gets $" (number->string amt)))]
    (cond
      [(empty? los) '()]
      [(and (cons? los) (empty? lon)) (cons (award (first los) 0)
                                            (prizes (rest los) lon))]
      [(and (cons? los) (cons? lon))  (cons (award (first los) (first lon))
                                            (prizes (rest los) (rest lon)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's actually implement our own version of the list abstraction sort.

; Exercise:
; Design the function mysort that accepts a list of numbers and
; returns a sorted version of that list.
; Use a list abstraction, but not sort.

; mysort : [List-of Number] -> [SortedList-of Number]
; Sorts a list of numbers
 
(check-expect (mysort '()) '())
(check-expect (mysort (list 9 2 5)) (list 2 5 9))
(check-expect (mysort (list 9 2 5 1 0 7)) (list 0 1 2 5 7 9))
 
(define (mysort lon)
  ;<DO NOW><SCROLL> helper: (insert n l)
  ; What type is l? Just a list, or something more?







  
  (local [; insert : Number [SortedList-of Number] -> [SortedList-of Number]
          ; Inserts the element at the right spot
          ; if given 3 and (list 1 2 4), returns (list 1 2 3 4)
          (define (insert n slon)
            (cond [(empty? slon) (list n)]
                  [(cons? slon) (if (< n (first slon))
                                    (cons n slon)
                                    (cons (first slon) (insert n (rest slon))))]))]
    (foldr insert '() lon)))

; So this is an example of foldr not only accepting a list, but
; returning a list!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function rev that accepts a list and reverses it.
; You must use a pre-defined list abstraction.

; rev : [List-of Any] -> [List-of Any]
; Reverse a list
 
(check-expect (rev '()) '())
(check-expect (rev (list 1 2 3)) (list 3 2 1))
(check-expect (rev (list "a" "b" "c")) (list "c" "b" "a"))
 
(define (rev lox)
  (foldl cons '() lox))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BUILD-LIST

; An actual practical application for build-list (eventually, in final version),
; within a program.
; Consider the following definition for an Uno playing card:
#|
(define-struct card [number color])
; An UnoCard is a (make-card Nat String)
|#
; Exercise:
; Design the function make-uno-deck that accepts a natural number
; representing the highest numeric card value and a list of strings,
; representing the colors, and generates a deck of Uno cards.
; There should be one card with each color and number.

; First do a version starting with templates, and then with list abstractions.

(define-struct card [number color])
; An UnoCard is a (make-card Nat String)

(define UNOCARD-1 (make-card 1 "red"))
(define UNOCARD-2 (make-card 2 "blue"))
 
(define (unocard-temp uc)
  (... (card-number uc) ...
       (card-color uc) ...))
 
; make-uno-deck : Nat [List-of String] -> [List-of UnoCard]
; Makes a deck of Uno cards, with one card of each color and number
 
(check-expect (make-uno-deck 3 (list "red" "green"))
              (list (make-card 1 "red") (make-card 2 "red") (make-card 3 "red")
                    (make-card 1 "green") (make-card 2 "green") (make-card 3 "green")))
 
(define (make-uno-deck/v1 num colors)
  ;<DO NOW><SCROLL>







  
  (local [; make-all-colors : [List-of String] -> [List-of UnoCard]
          ; Makes a deck of uno cards for each color for all num
          (define (make-all-colors loc)
            ;<DO NOW><SCROLL>







  
            (cond [(empty? loc) '()]
                  [(cons? loc) (append (make-all-nums (first loc) num)
                                       (make-all-colors (rest loc)))]))
 
          ; make-all-nums : String Nat -> [List-of UnoCard]
          ; Makes a deck of uno cards for a color for n and below
          (define (make-all-nums color n)
            ;<DO NOW><SCROLL>







  
            (cond [(zero? n) '()]
                  [(positive? n) (cons (make-card (add1 (- num n)) color)
                                       (make-all-nums color (sub1 n)))]))]
    (make-all-colors colors)))

; Notice that make-all-nums is essentially build-list and make-uno-deck is essentially foldr...
(define (make-uno-deck num colors)
  (local [; append-and-generate-color : String [List-of UnoCard] -> [List-of UnoCard]
          ; Appends all numbers for a color
          (define (append-and-generate-color color old-cards)
            (append (build-list num (λ (nat) (make-card (add1 nat) color)))
                    old-cards))]
    (foldr append-and-generate-color '() colors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ABSTRACTING OVER FUNCTIONS

; Consider the following data definition:

; A [Checklist Z] is a [List-of [Z -> Boolean]]
; Interpretation: A series of tests

; Exercise:
; Design the function checklist->predicate that accepts a [Checklist Z]
; and returns a single predicate that only returns true if all predicates
; in the checklist return true.

; A [Checklist Z] is a [List-of [Z -> Boolean]]
; Interpretation: A series of tests

; checklist->predicate : (Z) [Checklist Z] -> [Z -> Boolean]
; Returns a predicte that holds true only if all predicates return true

(check-expect ((checklist->predicate (list even? positive?)) -2) #false)
(check-expect ((checklist->predicate (list even? positive?)) 10) #true)
(check-expect (filter (checklist->predicate (list even? positive?))
                      (list -2 -1 0 1 2))
              (list 2))

; Let's break this down into two phases:
; - first, we design a way to apply a sequence of functions (predicates in this case)
; - then we design a way to bundle it up into a "meta-function"
; apply-all : (Z) [List-of [Z -> Boolean]] -> [List-of Boolean]
; Let's adapt the list template to our list-of-functions:
(check-expect (apply-all (list even? positive?) -2) (list #true #false))
(define (apply-all lop z)
  #;
  (cond [(empty? lop) ...]
        [(cons? lop) (... ((first lop) z) ...
                            (apply-all (rest lop) z) ...)])
  ;<DO NOW><SCROLL>







  
  (cond [(empty? lop) '()]
        [(cons? lop) (cons ((first lop) z)
                             (apply-all (rest lop) z))]))

; Maybe try using a list abstraction? We've usually applied a function param
; to a list of data--how about flipping the roles?
; apply-all/v2 : [List-of [X -> Boolean]] -> Boolean
; Determines if all tests pass
(check-expect (apply-all/v2 (list even? positive?) -2) #false)
(check-expect (apply-all/v2 (list even? positive?) 2) #true)

(define (apply-all/v2 lop x)
  (andmap (λ (p?) (p? x)) lop))

; How do we turn this into a standalone meta-predicate?
(λ (lop z)
  (andmap (λ (p?) (p? z)) lop))

; Finally, write a meta-predicate-creator, by taking advantage
; of scoping rules for functions
; NB: the param lop stays "in scope" for the lambda, and therefore the scope
; is preserved, even though the call to checklist->predicate returns
(define (checklist->predicate lop)
  (λ (z)
    (andmap (λ (p?) (p? z)) lop)))
