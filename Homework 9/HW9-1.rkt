;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

 (require racket/set)

; Exercise 1a
; diag-matrix : number number -> (listof (listof number))
; to produce a diagonal matrix of size n with d on the diagonal
(check-expect (diag-matrix 3 1) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (diag-matrix 4 2) (list (list 2 0 0 0) (list 0 2 0 0) (list 0 0 2 0) (list 0 0 0 2)))
(check-expect (diag-matrix 4 3) (list (list 3 0 0 0) (list 0 3 0 0) (list 0 0 3 0) (list 0 0 0 3)))

(define (diag-matrix n d)
    (local [(define (row i)
                (local [(define (col j)
                        (if (= i j) d 0))]
                (build-list n col)))]
        (build-list n row)))



; FIX
; Exercise 1b
; append-no-dups : [List-of X] [List-of X] (X) [X X -> Boolean] -> [List-of X]
; to produce a list of elements from the first list followed by the second list with no duplicates
(check-expect (append-no-dups (list 1 2 2 3) (list 3 4 5) =) (list 1 2 3 4 5))
(check-expect (append-no-dups (list 1 2 2 3) (list 3 4 5) =) (list 1 2 3 4 5))
(check-expect (append-no-dups (list #true #false) (list #false #true) boolean=?) (list #true #false))

(define (append-no-dups l1 l2 f)
    (cond
        [(empty? l1) l2]
        [(empty? l2) l1]
        [else (append-no-dups-helper l1 l2 f)]))

(define (append-no-dups-helper l1 l2 f)
    (cond
        [(empty? l1) l2]
        [else (append-no-dups-helper (rest l1) (remove-if (first l1) l2 f) f)]))

(define (remove-if a l2 f)
            (cond
                [(empty? l2) empty]
                [(f a (first l2)) (remove-if a (rest l2) f)]
                [else (cons (first l2) (remove-if a (rest l2) f))]))

(define (merge-lists l1 l2)
    (cond
        [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1) (merge-lists (rest l1) l2))]))

