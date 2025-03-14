;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname P1-collapse-step-demo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Today's agenda:
; Demo Stepper for:
;   (collapse (list "a" "b" "c") "" string-append)
;   (collapse (list 1 2 3 4) 10 -)
; Finish WORLDOFFORTUNE
; More list abstractions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define LON-0 '())
(define LON-1 (cons 2 LON-0))
(define LON-2 (cons 3 LON-1))
(define LON-3 (cons 4 LON-2))

; collapse : (X) [List-of X] X [X X -> X] -> X
; Collapses a list given a base-case and pairwise function
 
(check-expect (collapse LON-0 0 +) 0)
(check-expect (collapse LON-3 0 +) 9)
(check-expect (collapse LON-0 1 *) 1)
(check-expect (collapse LON-3 1 *) 24)
 
(define (collapse lon base f)
  (cond [(empty? lon) base]
        [(cons? lon) (f (first lon)
                        (collapse (rest lon) base f))]))


(collapse (list "a" "b" "c") "" string-append)

(collapse (list 1 2 3 4) 10 -)
