;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L17_P2-exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise: Consider the following function:

;; hello-all : [List-of String] -> [List-of String]
(check-expect (hello-all '()) '())
(check-expect (hello-all (cons "Alice" (cons "Bob" '()))) (cons "Hello, Alice!" (cons "Hello, Bob!" '())))

(define (hello-all los)
    (cond [(empty? los) '()]
          [(cons? los) (cons (string-append "Hello, " (first los) "!") 
                             (hello-all (rest los)))]))

;; add-ten-all : [List-of Number] -> [List-of Number]
(check-expect (add-ten-all '()) '())
(check-expect (add-ten-all (cons 10 (cons 1 '()))) (cons 20 (cons 11 '())))

(define (add-ten-all lon)
    (cond [(empty? lon) '()]
          [(cons? lon) (cons (+ 10 (first lon))
                             (add-ten-all (rest lon)))]))

; - Identify the differences between the two functions.
; - Write two helper functions, one for each function that abstracts away the  differences.
;  [DON'T WRITE THE ABSTRACTED FUNCTION YET]
; - Modify the two functions above to use the helpers.
;<DO NOW>








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise: Design an abstraction that you can use to rewrite the two functions
; given in the previous problem. The abstraction should use the two helpers that
; you wrote.
;<DO NOW>








