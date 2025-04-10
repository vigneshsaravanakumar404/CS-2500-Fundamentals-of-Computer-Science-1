;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ========================================================= IMPORTS
(define-struct btnode [value left right])
; A BinaryTree is one of
; - #false
; - (make-btnode Number BinaryTree BinaryTree)
; Interpretation: Represents a binary tree with nodes containing numbers
; Examples: <TO BE CREATED BY STUDENT>
(define (bt-temp bt)
  (cond [(boolean? bt) ...]
        [(btnode? bt) (... (btnode-value bt) ...
                           (bt-temp (btnode-left bt)) ...
                           (bt-temp (btnode-right bt)) ...)]))

; A [NEList-of X] is one of:
; - (cons X empty)
; - (cons X [NEList-of X])
; Intepretation: Represent a list of elements of type X which has at least one element
(define (nelox-temp nelox)
  (cond [(empty? (rest nelox)) (... (first nelox) ...)]
        [(cons? (rest nelox)) (... (first nelox) ...
                                   (nelox-temp (rest nelox)) ...)]))

; A ParString is a String over the alphabet {"("")"}
; representing an expression consisting of parentheses only.
(define PS-0 "")
(define PS-1 "()")
(define PS-2 "(())")
(define PS-3 "()()")
(define PS-4 "())(")
(define PS-5 "()(")
(define PS-6 "())")
(define PS-7 ")(")
; =================================================================
(define BT-0 #false)
(define BT-1 (make-btnode 10 (make-btnode 5 BT-0 BT-0) (make-btnode 17 BT-0 BT-0)))
(define BT-2 (make-btnode 31 (make-btnode 30 BT-0 BT-0) (make-btnode 34 BT-0 BT-0)))
(define BT-3 (make-btnode 16 BT-1 BT-2))


; Exercise 1
; valid-bst? : BinaryTree -> Boolean
; check if a binary tree is a valid binary search tree
(check-expect (valid-bst? BT-0) #t)
(check-expect (valid-bst? BT-1) #t)
(check-expect (valid-bst? BT-2) #t)
(check-expect (valid-bst? BT-3) #f)

(define (valid-bst? bst)
  (local [(define (valid-bst-helper bst min max)
            (if (boolean? bst) #t
                (and (<= min (btnode-value bst) max)
                  (valid-bst-helper (btnode-left bst) min (btnode-value bst))
                  (valid-bst-helper (btnode-right bst) (btnode-value bst) max))))]
    (valid-bst-helper bst -inf.0 +inf.0)))


; Exercise 2a
; index-of-max : [NEList-of Number] -> NonNegInt
; find the index of the maximum element
(check-expect (index-of-max (list 1 5 9 4 7 9 3)) 2)
(check-expect (index-of-max (list 9 3)) 0)
(check-expect (index-of-max (list 1 5 9 4 7 9)) 2)

(define (index-of-max lst)
  (local [
          ; accumulator : [NEList-of Number] NonNegInt Number NonNegInt -> NonNegInt
          ; find the index of the maximum element in a list
          ; lst=(list 9 3) i=5 max=9 max-1=2 should return 2
          ; lst=(list 1 5 9 4 7 9 3) i=0 max=1 max-1=0 should return 2
          (define (accumulator lst i max max-1)
            (cond [(empty? lst) max-1]
                  [(> (first lst) max) (accumulator (rest lst) (+ i 1) (first lst) i)]
                  [else (accumulator (rest lst) (+ i 1) max max-1)]))]
    (accumulator lst 0 (first lst) 0)))

; Exercise 2b
; balanced-paren? : ParString -> Boolean
; check if the parentheses are balanced
(check-expect (balanced-paren? PS-0) #t)
(check-expect (balanced-paren? PS-1) #t)
(check-expect (balanced-paren? PS-2) #t)
(check-expect (balanced-paren? PS-3) #t)
(check-expect (balanced-paren? PS-4) #f)
(check-expect (balanced-paren? PS-5) #f)
(check-expect (balanced-paren? PS-6) #f)
(check-expect (balanced-paren? PS-7) #f)
(check-expect (balanced-paren? "(()())") #t)
(check-expect (balanced-paren? "(()))") #f)
(check-expect (balanced-paren? "(()(())))") #f)

(define (balanced-paren? lst)
  (local [; accumulator : [NEList-of String] NonNegInt -> Boolean
          ; check if the parentheses are balanced
          ; lst=(list "(" ")" "(" ")" "(" ")" "(" ")" "(" ")" "(" ")" "(" ")") c=0 should return #t
          ; lst=(list "(") c=1 should return #f
          ; lst=(list ")") c=1 should return #t
          (define (accumulator lst c)
            (cond
              [(empty? lst) (= c 0)]
              [(string=? (first lst) "(") (accumulator (rest lst) (+ c 1))]
              [(> c 0) (accumulator (rest lst) (- c 1))]
              [else #f]))]
  (accumulator (explode lst) 0)))

; Exercise 2c
; fibonacci : NonNegInt -> NonNegInt
; compute the nth Fibonacci number
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 10) 55)
(check-expect (fibonacci 11) 89)
(check-expect (fibonacci 341) 82281144336295989585340713815384441479925901307982452831610787275979941)

(define (fibonacci n)
  (local [; accumulator : NonNegInt NonNegInt NonNegInt NonNegInt -> NonNegInt
          ; compute the nth Fibonacci number using an accumulator
          ; n=11 i=11 a=0 b=1 should return 89
          ; n=10 i=0 a=0 b=1 should return 55
          (define (accumulator n i a b)
            (if (= i n) a (accumulator n (+ i 1) b (+ a b))))]
    (accumulator n 0 0 1)))