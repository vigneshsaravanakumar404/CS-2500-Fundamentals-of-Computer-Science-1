;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname L28_P1-accum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ACCUMULATORS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOTIVATING EXAMPLE: ADDING UP NUMBERS
;
; Let's imagine we have a list of relative distances between cities along a road,
; and we want to convert them to a list of absolute distances from the first city.
; For example, we might have...
;
;   (list 10 50 20)
;
; and we would want to convert this to...
;
;   (list 10 60 80)
;
; This would help us plan out our stop for gas along the way


; Exercise:
; Design this as the function relative->absolute using structural recursion.

; relative->absolute : [List-of Number] -> [List-of Number]
; Converts the relative distances to absolute ones

(check-expect (relative->absolute '())
              '())

(check-expect (relative->absolute (list 10 50 20))
              (list 10 60 80))

;<BRAINSTORM>








(define (relative->absolute/v1 lon)
  (local [; add-to-each : Number [List-of Number] -> [List-of Number]
          ; Adds the supplied number to every element
          (define (add-to-each num mylon)
            (cond [(empty? mylon) '()]
                  [(cons? mylon)  (cons (+ num (first mylon))
                                        (add-to-each num (rest mylon)))]))]
    (cond [(empty? lon) '()]
          [(cons? lon)  (cons (first lon)
                              (add-to-each (first lon)
                                           (relative->absolute/v1 (rest lon))))])))

; How well does this function perform? Try this:

#;(time (length (relative->absolute/v1 (map random (build-list 5000 add1)))))

; Not great. Let's think about why.
; Is there a better way?

;<BRAINSTORM>









(define (relative->absolute/v2 lon running-total)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (+ (first lon) running-total)
                           (relative->absolute/v2 (rest lon)
                                                  (+ (first lon) running-total)))]))

; This running total is called the accumulator. But now the signature has changed,
; and furthermore, involves a parameter the user doesn't understand.
; How can we "hide" the accumulator?

(define (relative->absolute lon)
  (local [; relative->absolute/a : [List-of Number] Number -> [List-of Number]
          ; Converts the distances from relative to absolute
          ; Accumulator: the total distance so far
          (define (relative->absolute/a lon acc)
            (cond [(empty? lon) '()]
                  [(cons? lon) (cons (+ (first lon) acc)
                                     (relative->absolute/a (rest lon)
                                                           (+ (first lon) acc)))]))]
    (relative->absolute/a lon 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designing with accumulators requires only changing the template
; and then adding a statement. In the case of accumulators, the template becomes...

#;
(define (foo x y z)
  (local [; SIG
          ; PURPOSE
          ; Accumulator: MEANS
          ; TESTS
          (define (foo/a x y z acc)
            ...)]
    (foo/a x y z acc0)))

; The reason for this slightly strange nested form is that we do not want the end-user to have
; to concern themselves with the accumulator, which is more of an implementation detail.

; NB: It is very important to remember to update the accumulator before passing it in
; to the recursive call.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design digits->number, which consumes a list of digits and produces the corresponding number.
; For example, when applied to (list 1 0 2), it produces 102.

; First design without an accumulator, and then with one.


; digits->number : [List-of Nat] -> Nat
; Converts a list of digits into the corresponding number
; in base 10
 
(check-expect (digits->number '()) 0)
(check-expect (digits->number (list 0)) 0)
(check-expect (digits->number (list 1 0 2)) 102)
#;
(define (digits->number lon)
  (cond [(empty? lon) 0]
        [(cons? lon)  (+ (* (first lon) (expt 10 (length (rest lon))))
                         (digits->number (rest lon)))]))


; Notice that each recursive call has to re-determine its exponent. This in turn means
; each call needs to count up how many elements are in the list. Can we avoid this?
#;
(define (digits->number lon)
  (local [; digits->number/a : [List-of Nat] Nat -> Nat
          ; Converts the list of digits to a base10 number
          ; Accumulator: the current multiplier
          (define (digits->number/a l acc)
            (cond [(empty? l) 0]
                  [(cons? l) (+ (* (first l) acc)
                                (digits->number/a (rest l) (/ acc 10)))]))]
    (digits->number/a lon (expt 10 (sub1 (length lon))))))

; Can we do *even* better??
; Consider: if you were converting (list 1 2 3 4), and I was revealing the
; digits to you one at a time from left to right (what the recursion does),
; and you had only seen 1 and 2 so far, what would you think the number was?
; And when I reveal that there is another digit, 3, how would you re-think
; what the 1 and 2 you had already seen stood for? How could you use that?
;<BRAINSTORM>








(define (digits->number lon)
  (local [; digits->number/a : [List-of Nat] Nat -> Nat
          ; Converts the list of digits to a base10 number
          ; Accumulator: the current converted number
          (define (digits->number/a l acc)
            (cond [(empty? l) acc]
                  [(cons? l) (digits->number/a (rest l)
                                               (+ (* 10 acc) (first l)))]))]
    (digits->number/a lon 0)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IMPLEMENTING FOLDL

; How does foldl work? Let's find out by implementing it ourselves!
; Reimplementing the wheel can be fun, as well as educational...

; Quick review of foldl:
#|
(foldl f base (cons A (cons B (cons C '()))))
=
              (f    C (f    B (f    A base)))
vs foldr which ...

(foldr f base (cons A (cons B (cons C '()))))
=
              (f    A (f    B (f    C base)))
|#

; What would the following expressions produce?
;(foldr string-append "XXX" (list "a" "b" "c"))
;(foldl string-append "XXX" (list "a" "b" "c"))

; Exercise:
; Design the function f0ldl, that accepts a function [X Y -> Y], a Y,
; and a list [List-of X] and returns a Y.


; f0ldl : (X Y) [X Y -> Y] Y [List-of X] -> Y
; Iterative applies the function starting from the left

(check-expect (f0ldl string-append "XXX" '()) "XXX")
(check-expect (f0ldl string-append "XXX" (list "a" "b" "c")) "cbaXXX")

#;
(define (f0ldl f base lox)
  (local [; foldl/a : (X Y) [X Y -> Y] Y [List-of X] ??? -> Y
          ; Implements foldl, given the accumulator
          ; Accumulator: ???
          (define (foldl/a f base lox ???)
            ...)]
    (foldl/a f base lox ???)))

; How do we want to think about the accumulator?
; It's just the elements of the list so far that have been "folded" (crunched,
; as I like to say) into f, so:
#;
(define (f0ldl f base lox)
  (local [; f0ldl/a : (X Y) [X Y -> Y] [List-of X] Y -> Y
          ; Implements foldl, given the accumulator
          ; Accumulator:  the previous elements all crunched together
          (define (f0ldl/a f lox acc)
            (cond [(empty? lox) acc]
                  [(cons? lox) (f0ldl/a f (rest lox) (f (first lox) acc))]))]
    (f0ldl/a f lox base)))


; Is the local scope serving any real purpose? Can we simplify?

(define (f0ldl f base lox)
  (cond
    [(empty? lox) base]
    [(cons? lox)
     (f0ldl f (f (first lox) base) (rest lox))]))

; Basically, after each step, the integrated result of f becomes the next base.
