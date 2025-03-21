;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lab 9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; alternate : [list-of-X] [list-of-Y] -> [list-of-(union X Y)]
; to produce a list that alternates elements from l1 and l2
(check-expect (alternate '() '()) '())
(check-expect (alternate '() (list "a" "b")) (list "a" "b"))
(check-expect (alternate (list 1 2 3) '()) (list 1 2 3))
(check-expect (alternate (list 1 2) (list "a" "b" "c"))
              (list 1 "a" 2 "b" "c"))

(define (alternate l1 l2)
  (local [(define (alternate-helper l1 l2 i)
            (cond
              [(empty? l1) l2]
              [(empty? l2) l1]
              [(even? i) (cons (first l1) (alternate-helper (rest l1) l2 (+ i 1)))]
              [else (cons (first l2) (alternate-helper l1 (rest l2) (+ i 1)))]))]
    (alternate-helper l1 l2 0)))



; cross-product : [list-of-X] [list-of-Y] -> [list-of-[X Y]]
; to produce the cross-product of l1 and l2
(check-expect (cross-product '() '()) '())
(check-expect (cross-product '() (list "a" "b")) '())
(check-expect (cross-product (list 1 2 3) '()) '())
(check-expect (cross-product (list "a" "b" "c") (list "a" "b" "c"))
              (list (list "a" "a") (list "a" "b") (list "a" "c")
                    (list "b" "a") (list "b" "b") (list "b" "c")
                    (list "c" "a") (list "c" "b") (list "c" "c")))
(check-expect (cross-product (list 1 2) (list "a" "b" "c"))
              (list (list 1 "a") (list 1 "b") (list 1 "c")
                    (list 2 "a") (list 2 "b") (list 2 "c")))

(define (cross-product l1 l2)
  (local [(define (recurse-l1 l1 l2)
            (cond
              [(empty? l1) empty]
              [else (append (recurse-l2 (first l1) l2)
                            (recurse-l1 (rest l1) l2))]))
          (define (recurse-l2 element l2)
            (cond
              [(empty? l2) empty]
              [else (cons (list element (first l2))
                          (recurse-l2 element (rest l2)))]))]
    (recurse-l1 l1 l2)))
    
