;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L16_P2-abst-funcs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

; Exercise 1
(define LST-TRI (cons (triangle 20 "solid" "green")
                      (cons (triangle 35 "solid" "red")
                            (cons (triangle 45 "solid" "orange")
                                  (cons (triangle 55 "solid" "blue") '())))))
(define LST-TRI2 (cons (triangle 20 "solid" "blue")
                       (cons (triangle 35 "solid" "red")
                             (cons (triangle 45 "solid" "orange")
                                   (cons (triangle 55 "solid" "blue") '())))))
(define LST-TRI3 (cons (triangle 20 "solid" "yellow")
                       (cons (triangle 35 "solid" "red")
                             (cons (triangle 45 "solid" "orange")
                                   (cons (triangle 55 "solid" "blue") '())))))
; Using LST-TRI in our merge-layers function should output an image of a concentric
; set of triangles, much like a triangular bullseye.

; merge-layers : LST -> Image
; Merges a number of image layers together into one image by overlaying them
(check-expect (merge-layers LST-TRI)
              (overlay
               (triangle 20 "solid" "green")
               (triangle 35 "solid" "red")
               (triangle 45 "solid" "orange")
               (triangle 55 "solid" "blue")))
(check-expect (merge-layers LST-TRI2)
              (overlay
               (triangle 20 "solid" "blue")
               (triangle 35 "solid" "red")
               (triangle 45 "solid" "orange")
               (triangle 55 "solid" "blue")))
(check-expect (merge-layers LST-TRI3)
              (overlay
               (triangle 20 "solid" "yellow")
               (triangle 35 "solid" "red")
               (triangle 45 "solid" "orange")
               (triangle 55 "solid" "blue")))
(define (merge-layers lst)
  (cond
    [(empty? lst) empty-image]
    [(cons? lst) (overlay (first lst) (merge-layers (rest lst)))]))

; Exercise 2
; matching-x-posn : [List-of Posn] Number Posn -> Posn
; Find the first Posn in the list with the given x-coordinate or return the given Posn
; if no such position can be found
(check-expect (matching-x-posn '() 10 (make-posn 0 0)) (make-posn 0 0))
(check-expect
 (matching-x-posn
  (cons (make-posn 1 2) (cons (make-posn 3 4) '())) 3 (make-posn 5 6))
 (make-posn 3 4))
(check-expect
 (matching-x-posn
  (cons
   (make-posn 1 5)
   (cons (make-posn 1 2) (cons (make-posn 3 4) '()))) 10 (make-posn 0 0)) (make-posn 0 0))

(define (matching-x-posn lop desired-x default)
  (find-first-match lop desired-x posn-x default))
 
; string-with-length : [List-of String] Nat -> String
; Returns the first String in the given list with the given length or "no such string" if no
; such string can be found
(check-expect (string-with-length '() 10) "no such string")
(check-expect (string-with-length (cons "hi" (cons "hello" (cons "aloha" '()))) 5) "hello")
(check-expect (string-with-length (cons "bye" '()) 3) "bye")

(define (string-with-length los desired-length)
  (find-first-match los desired-length string-length "no such string"))

; find-first-match : [List-of Any] -> Any
(check-expect (find-first-match '() 10 string-length  "no such string") "no such string")
(check-expect
 (find-first-match
  (cons "hi" (cons "hello" (cons "aloha" '()))) 5 string-length "no such string") "hello")
(check-expect (find-first-match (cons "bye" '()) 3 string-length "no such string") "bye")
(define (find-first-match loa desired func-to-use default)
  (cond [(empty? loa) default]
        [(cons? loa)
         (if (= (func-to-use (first loa)) desired)
             (first loa)
             (find-first-match (rest loa) desired func-to-use default))]))


