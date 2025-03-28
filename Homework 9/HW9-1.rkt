;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; Exercise 1a
; diag-matrix : NonNegInt Number -> [List-of [List-of Number]]
; to produce a diagonal matrix of size n with d on the diagonal
(check-expect (diag-matrix 3 1) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (diag-matrix 3 0) (list (list 0 0 0) (list 0 0 0) (list 0 0 0)))
(check-expect (diag-matrix 4 2) (list (list 2 0 0 0) (list 0 2 0 0) (list 0 0 2 0) (list 0 0 0 2)))
(check-expect (diag-matrix 4 3) (list (list 3 0 0 0) (list 0 3 0 0) (list 0 0 3 0) (list 0 0 0 3)))
(check-expect (diag-matrix 0 1) empty)

(define (diag-matrix n d)
  (local [
          ; row : Number -> (Listof Number)
          ; Produces a list of size n with d at index i and 0 elsewhere
          ; (row 0) and n = 4 should return (d 0 0 0)
          ; (row 1) and n = 4 should return (0 d 0 0)
          ; (row 2) and n = 4 should return (0 0 d 0)
          ; (row 3) and n = 4 should return (0 0 0 d)
          (define (row i)
            (local [
                    ; col : Number -> Number
                    ; Determines value at index j in the row: d if i == j, otherwise 0
                    ; (col 3) and n = 4 should return (0 0 0 d)
                    ; (col 2) and n = 4 should return (0 0 d 0)
                    ; (col 1) and n = 4 should return (0 d 0 0)
                    ; (col 0) and n = 4 should return (d 0 0 0)
                    (define (col j)
                      (if (= i j) d 0))]
              (build-list n col)))]
    ; Builds a matrix of size n x n with diagonal elements as d
    (build-list n row)))



; Exercise 1b
; append-no-dups : (X) [List-of X] [List-of X] [X X -> Boolean] -> [List-of X]
; to produce a list of elements from the first list followed by the second list with no duplicates
; note that this implementation drops the first occurrence of a duplicate element not the last
(check-expect (append-no-dups (list 1 2 2 3) (list 3 4 5) =) (list 1 2 3 4 5))
(check-expect (append-no-dups (list 1 2 3 3 3) empty =) (list 1 2 3))
(check-expect (append-no-dups empty (list 1 2 3 3 3) =) (list 1 2 3))
(check-expect (append-no-dups (list 1 2 3 4 5) (list 6 7 99) =) (list 1 2 3 4 5 6 7 99))
(check-expect (append-no-dups (list "a" "b" "c" "b") (list "d" "b" "e") string=?) 
              (list "a" "c" "d" "b" "e"))
(check-expect (append-no-dups (list 1 2 3 3 3) (list 1 2 333) =) (list 3 1 2 333))
(check-expect (append-no-dups (list #true #false) (list #false #true) boolean=?) (list #false #true))


(define (append-no-dups l1 l2 f)
  (local [
          ; remove-duplicates : (X) [List-of X] (X X -> Boolean) -> [List-of X]
          ; to produce a list of elements from the first list followed by the second 
          ; list with no duplicates
          ; (remove-duplicates (list 1 2 3) =) should return (list 1 2 3)
          ; (remove-duplicates (list 1 2 3 3) =) should return (list 1 2 3)
          (define (remove-duplicates l f)
            (cond
              [(empty? l) empty]
              [(is-duplicate? (first l) (rest l)) (remove-duplicates (rest l) f)]
              [else (cons (first l) (remove-duplicates (rest l) f))]))
              
          ; is-duplicate? : (X) [List-of X] (X X -> Boolean) -> Boolean
          ; to produce true if x is in lst and false otherwise
          ; (is-duplicate? 1 (list 2 3 4)) should return #false
          ; (is-duplicate? 1 (list 1 2 3)) should return #true
          (define (is-duplicate? x lst)
            (cond
              [(empty? lst) #false]
              [(f x (first lst)) #true]  
              [else (is-duplicate? x (rest lst))]))]
    (remove-duplicates (append l1 l2) f)))
