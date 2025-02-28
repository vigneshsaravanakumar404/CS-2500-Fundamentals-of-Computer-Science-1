#lang htdp/isl

; 1. 
; f : Number Number Number -> Number
; Computes the sum of x, y, and z, where x is the sum of y and z, y is z, and z is the product of x and y.
(check-expect (f 1 2 3) 23)

(define (f x y z)
  (local [(define x (+ y z))]
    (local [(define y z)]
      (local [(define z (* x y))]
        (+ x y z)))))
 
 
; g : String Boolean -> Number
; Computes the sum of the length of a and 10 if b is true, 5 otherwise.
(check-expect (g "5" true) 31)

(define (g a b)
  (+ (local [(define b (string->number a))]
       (local [(define a (sqr b))]
         (- a b)))
     (string-length a)
     (if b 10 5)))
 

; 2.
; exercise-2 : Number [List of] -> Number
; multiplies each number in the list by a given value if the number is positive—otherwise 
; keeps the number as is and returns the sum of the resulting list using one list abstraction
(check-expect (exercise-2 (list 1 2 3 4 5) 2) 30)
(check-expect (exercise-2 (list -1 -2 -3 -4 -5) 2) -15)
(check-expect (exercise-2 (list 1 2 3 4 5) 0) 0)
(check-expect (exercise-2 (list 1 2 3 4 5) -1) -15)

(define (exercise-2 loN n)
  (local [(define (helper x y)
            (+ (if (> x 0) (* x n) x) y))]
    (foldr helper 0 loN)))


; 3.
; numlist-intersect : [List-of Number] [List-of Number] -> [List-of Number]
; Returns the intersect of both lists
(check-expect (numlist-intersect (list 1 2 3 4) (list 5 6 7 8)) '())
(check-expect (numlist-intersect (list 1 2 4 5) (list 4 5 7 8)) (list 4 5))
(check-expect (numlist-intersect (list 1 2 3 4) (list 1 2 3 4)) (list 1 2 3 4))
(define (numlist-intersect l1 l2)
  (local [(define (is_in? x)
            (member x l2))]
    (filter is_in? l1)))

; 4.
; intersect : (X) [List-of X] [List-of X] -> [List-of X]
; Returns the intersect of both lists
(check-expect (intersect (list 1 2 3 4) (list 5 6 7 8)) '())
(check-expect (intersect (list "blob" 2 4 5) (list "blob" 5 7 8)) (list "blob" 5))
(check-expect (intersect (list "a" #true 69 "blob") (list "a" #true 69 "blob"))
              (list "a" #true 69 "blob"))
(define (intersect l1 l2)
  (local [(define (is_in? l)
            (member l l2))]
    (filter is_in? l1)))


; 5.
; remove-all-matches : [List of X] X [X X -> Boolean] -> [List of X]
; Removes all elements from a list that match a given value according to a given predicate
(check-expect (remove-all-matches (list 1 2 3 4 5) 3 =) (list 1 2 4 5))
(check-expect (remove-all-matches (list 1 2 3 4 5) 3 <) (list 3 4 5))
(check-expect (remove-all-matches (list 1 2 3 4 5) 3 >) (list 1 2 3))

(define (remove-all-matches loX x pred)
  (local [(define (helper y)
            (not (pred y x)))]
    (filter helper loX)))

; 6.
; add-all-matches : [List of X] X [X X -> Boolean] -> [List of X]
; duplicates any element that is equal to the given element based on prediate
(check-expect (add-all-matches (list 1 2 1) 1 =) (list 1 1 2 1 1))
(check-expect (add-all-matches (list 1 2 1) 1 <) (list 1 2 1))
(check-expect (add-all-matches (list 1 2 1) 1 >) (list 1 2 2 1))

(define (add-all-matches loX z pred)
  (local [(define (helper y acc)
            (if (pred y z) 
                (append acc (list y y)) 
                (append acc (list y))))]
    (foldr helper '() loX)))  ; Move inside local
