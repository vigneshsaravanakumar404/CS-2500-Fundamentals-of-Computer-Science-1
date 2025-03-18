;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-distrib) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2:

;; Part A: Design the function count-trues

; count-trues : [List-of Boolean] -> NatNum
; Produces the count of #true values in the list
(define (count-trues lob)
  (cond [(empty? lob) 0]
        [(cons? lob) (if (first lob)
                           (add1 (count-trues (rest lob)))
                           (count-trues (rest lob)))]))

;; Part B: Design the function nth-is-true?

; nth-is-true? : [List-of Boolean] NatNum -> Boolean
;; returns whether the value at position n (counting from 0) is #true.
(define (nth-is-true? lob n)
  (cond [(empty? lob) #false]
        [(cons? lob) (if (zero? n)
                          (first lob)
                          (nth-is-true? (rest lob) (sub1 n)))]))

;; Part C: Design the function first-true

; first-true : [List-of Boolean] -> NatNum
; returns the position (counting from 0) of the first #true, or -1 if none were found.
(define (first-true lob)
  (cond [(empty? lob) -1]
        [(cons? lob) (if (first lob)
                          0
                          (if (= -1 (first-true (rest lob)))
                              -1
                              (add1 (first-true (rest lob)))))]))

;; Part D: Design the function set-true

; set-true : [List-of Boolean] NatNum -> [List-of Boolean]
; returns the list with the n-th item in the list converted from #false to #true
(define (set-true lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #true (rest lob))
                          (cons (first lob) (set-true (rest lob) (sub1 n))))]))


;; Part E: Design the function set-false

; set-false : [List-of Boolean] NatNum -> [List-of Boolean]
; returns the list with the n-th item in the list converted from #true to #false
(define (set-false lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #false (rest lob))
                          (cons (first lob) (set-false (rest lob) (sub1 n))))]))

;; Part F: Design the function draw-map

(require 2htdp/image)

(define BOX-HEIGHT 16)

; draw-map : [List-of Boolean] -> Image
; displays the list as a series of contiguous rectangles of the requested width width:height ratio.
;; render the #true values as black filled boxes, and #false values as black-outlined white boxes.
(define (draw-map lob n)
  (cond [(empty? lob) empty-image]
        [(cons? lob) (beside (if (first lob)
                                 (overlay (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "outline" "black")
                                          (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "solid" "black"))
                                 (overlay (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "outline" "black")
                                          (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "solid" "white")))
                             (draw-map (rest lob) n))]))
