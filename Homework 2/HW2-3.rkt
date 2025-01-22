#lang htdp/bsl
(require 2htdp/universe)
(require 2htdp/image)

;; ====================================================
;; Exercise 3

;; next-collatz : Number -> Number
;; Given a number n, returns the next number in the Collatz sequence.
;; Example: (next-collatz 5) => 16
(define (next-collatz n)
  (cond 
    [(= n 1) 1]
    [(= (remainder n 2) 0) (/ n 2)]
    [else (+ (* 3 n) 1)]))


;; collatz : Number -> String
;; Given a number n, returns a string that explains the next number in the Collatz sequence.
;; Example: (collatz 5) => "12 is even, so next: 12 / 2 = 6"
(define (collatz n)
  (string-append (number->string n) 
                 (cond 
                   [(= n 1) " is 1, so done"]
                   [(= (remainder n 2) 0) (string-append
                                           " is even, so next: "
                                           (number->string n)
                                           " / 2 = "
                                           (number->string
                                            (next-collatz 2)))]
                   [else (string-append " is odd, so next: 3 * "
                                        (number->string n)
                                        " + 1 = "
                                        (number->string
                                         (next-collatz n)))])))

;; handle-key : Number String -> Number
;; Given a number and a string, returns the next number in the Collatz sequence.
;; Example: (handle-key 5 "right") => 16
(define (handle-key state key)
  (cond
    [(string=? key "right") (next-collatz state)]
    [else state]))

;; render : Number -> Image
;; Given a number n, returns an image that displays the next number in the Collatz sequence.
;; Example: (render 5) => (text "12 is even, so next: 12 / 2 = 6" 15 "black")
(define (render state)
  (overlay
   (rectangle 500 200 "outline" "lightblue")
   (text (collatz state) 32 "black")))

(big-bang 20
  (on-key handle-key)
  (to-draw render))

;; Tests
;; (next-collatz 27) ; should be 82
;; (next-collatz 82) ; should be 41
;; (next-collatz 2) ; should be 1
;; ====================================================