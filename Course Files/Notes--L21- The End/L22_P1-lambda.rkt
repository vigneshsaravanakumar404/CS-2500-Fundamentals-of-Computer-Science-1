;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L22_P1-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Previously, we defined define (pun intended!) as having two different purposes:

(define FOO 3)

; and...

(define (foo x) (+ x 18))

; Today, we will unify them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LAMBDA (λ)

; Let's start by reviewing some functions from earlier:

; double-squares : Nat -> [List-of Nat]
; The first n double squares

(check-expect (double-squares 0) '())
(check-expect (double-squares 1) (list 0))
(check-expect (double-squares 4) (list 0 2 8 18))
 
(define (double-squares n)
  (local [; double-square : Nat -> Nat
          ; The nth double square
          (define (double-square n)
            (* 2 (sqr n)))]
    (build-list n double-square)))


; add-3-to-all : [List-of Number] -> [List-of Number]
; Adds 3 to every number

(check-expect (add-3-to-all '()) '())
(check-expect (add-3-to-all (list 0)) (list 3))
(check-expect (add-3-to-all (list -7 2.3)) (list -4 5.3))

(define (add-3-to-all lon)
  (local [; add-3 : Number -> Number
          ; Adds 3 to a number
          ; Given 4, should return 7
          (define (add-3 n)
            (+ n 3))]
    (map add-3 lon)))

; Examine the double-square and add-3 functions. Both are relatively trivial,
; but we had to go through the entire design recipe for them, which seems excessive.
; We would just fit this function inside the list abstraction if we could...

; We can do that with lambda. It is the essence of functions.

; Vocabulary	lambda
; Grammar	(lambda (x1 x2 ...) expression)
; Scope		x1, etc are only valid inside of expression
; Semantics	Creates a function. When the function is applied to "vx1, ..."
;		its value becomes the value of the expression with every x1 replaced by vx1, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To use lambda you have to use a new language: ISL+.
; (Time to raise the training wheels even higher again!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following is the most trivial example:
(lambda (n) (+ n 3)) ; a + 3 function

; How do I use a lambda? Well, one option is to pass it in as an argument to a function.
(map (lambda (n) (+ n 3)) (list 1 2 3))

; You can also use the λ symbol (ctrl-\ ; looks cooler!)
(map (λ (n) (+ n 3)) (list 1 2 3))

; You can also apply it directly (ISL still fenced this off for safety, but ISL+λ... OK!
((λ (n) (+ n 3)) 7)
((λ (x y) (+ x y)) 7 15)
((λ (x y) 4) 7 15)

; What does the following function do?

; weird : ? -> ?
(define (weird x)
  ((if (even? x)
       (λ (x) 10)
       (λ (x) (+ (sqr x) 1))) x))

; NB: just like any superpower, don't abuse this! (e.g.: Brightburn!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NAMELESS FUNCTIONS

; What lambda does is create a function without a name: in a sense, raw code.
; We already saw that it qualifies as a helper function for list abstractions.

; Exercise:
; Design a function short-messages that accepts a [List-of String] and
; returns those that are shorter than 14 characters.

; short-msgs : [List-of Strings] -> [List-of Strings]
; Keeps just the strings that are < 14 characters

(define SHORT-LEN 14)

(check-expect (short-msgs (list)) (list))

(check-expect (short-msgs (list "a" "b" "hi" "12345678901234567"))
              (list "a" "b" "hi"))
 
(define (short-msgs los)
  ;<DO NOW> helper: abstraction + short-msg?







  
  (filter short-msg? los))

; short-msg? : String -> Boolean
; Returns whether or not the message is < 14 chars
 
(check-expect (short-msg? "hi") #true)
(check-expect (short-msg? "123456789012345") #false)
 
(define (short-msg? msg)
  (< (string-length msg) SHORT-LEN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Re-implement short-msg? as a lambda.
;<DO NOW><SCROLL>








(λ (msg) (< (string-length msg) SHORT-LEN))

; Exercise:
; Re-design short-msgs with this helper.

(define (short-msgs/v2 los)
  (filter (λ (msg) (< (string-length msg) SHORT-LEN))
          los))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function product-sqrts-plus-one that accepts a [List-of Number] and
; returns the product of (the sqrt of each number plus one).
; Look for opportunities to use lambda

; product-sqrts-plus-one : [List-of Number] -> Number
; returns the product of (the sqrt of each number plus one)

(check-expect (product-sqrts-plus-one (list)) 1)
(check-expect (product-sqrts-plus-one (list 1 4 9)) 24)
 
(define (product-sqrts-plus-one lon)
  ; What list abstraction(s) should we use?
  ;<DO NOW><SCROLL>







  
  (foldr * 1
         (map (λ (x) (+ (sqrt x) 1)) lon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BACK TO DEFINE
; Review the original two forms of define usage

; What does define do? It binds a name to a value.
; Functions (the code for them) are values, too!
; Thus, can we do...
#;
(define f (λ (x) (+ x 7)))





; It turns out that this is exactly what the following does:
#;
(define (f x) (+ x 7))

; So, this original way we defined functions is actually just a shorthand;
; lambda is the one and only way to create a function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note that it's possible to do return functions via local as well...
#;
(define f
  (local [(define (f1 x) (+ x 7))]
    f1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTING FUNCTIONS THAT RETURN FUNCTIONS

; Reminder: we can't directly test equality between functions

(define (f x) (+ x 7))
(define fl (λ (x) (+ x 7)))
 
#;
(check-expect f fl)  ; would cause error

; Better way: check for equality of functionality

(check-expect (f 5) (fl 5))
