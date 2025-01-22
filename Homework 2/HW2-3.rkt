#lang htdp/bsl
(require 2htdp/universe)
(require 2htdp/image)

;; Exercise 3

;; next-collatz : Number -> Number
;; Given a number n, returns the next number in the collatz sequence.
(check-expect (next-collatz 27) 82)
(check-expect (next-collatz 82) 41)
(check-expect (next-collatz 2) 1)
(check-expect (next-collatz 1) 1)

(define (next-collatz n)
  (cond 
    [(= n 1) 1]
    [(= (remainder n 2) 0) (/ n 2)]
    [else (+ (* 3 n) 1)]))


;; print-text : Number -> String
;; Given a number n, returns a string that explains the next number in the collatz sequence.
(check-expect (print-text 27) "27 is odd, so next: 3 * 27 + 1 = 82")
(check-expect (print-text 82) "82 is even, so next: 82 / 2 = 41")
(check-expect (print-text 2) "2 is even, so next: 2 / 2 = 1")
(check-expect (print-text 1) "1 is reached so the conjecture holds")

(define (print-text n)
  (string-append (number->string n) 
                 (cond 
                   [(= n 1) " is reached so the conjecture holds"]
                   [(= (remainder n 2) 0) (string-append
                                           " is even, so next: "
                                           (number->string n)
                                           " / 2 = "
                                           (number->string
                                            (next-collatz n)))]
                   [else (string-append " is odd, so next: 3 * "
                                        (number->string n)
                                        " + 1 = "
                                        (number->string
                                         (next-collatz n)))])))

;; handle-key : Number String -> Number
;; Given a number and key input, returns the next number in the collatz sequence.
(check-expect (handle-key 27 "\r") 82)
(check-expect (handle-key 82 "\r") 41)
(check-expect (handle-key 2 "\r") 1)
(check-expect (handle-key 2 "right") 2)

(define (handle-key state key)
  (cond
    [(string=? key "\r") (next-collatz state)]
    [else key]))
    

;; render : Number -> Image
;; Given a number n, returns an image that displays the next number in the collatz sequence.
(check-expect (render 27) (overlay
                           (rectangle 500 200 "outline" "lightblue")
                           (text "27 is odd, so next: 3 * 27 + 1 = 82" 12 "black")))
(check-expect (render 82) (overlay
                           (rectangle 500 200 "outline" "lightblue")
                           (text "82 is even, so next: 82 / 2 = 41" 12 "black")))

(define (render state)
  (overlay
   (rectangle 500 200 "outline" "lightblue")
   (text (print-text state) 12 "black")))

;; collatz : Number -> WorldState
;; Simulates the collatz conjecture with explanations for each step.
(define (collatz n)
  (big-bang n
    (on-key handle-key)
    (to-draw render)))

(collatz 989345275647)