#lang htdp/bsl
(require 2htdp/universe)
(require 2htdp/image)

;; ====================================================
;; Exercise 3

;; next-collatz : Number -> Number
;; Given a number n, returns the next number in the Collatz sequence.
(check-expect (next-collatz 27) 82)
(check-expect (next-collatz 82) 41)
(check-expect (next-collatz 2) 1)
(check-expect (next-collatz 1) 1)

(define (next-collatz n)
  (cond 
    [(= n 1) 1]
    [(= (remainder n 2) 0) (/ n 2)]
    [else (+ (* 3 n) 1)]))


;; collatz : Number -> String
;; Given a number n, returns a string that explains the next number in the Collatz sequence.
(check-expect (collatz 27) "27 is odd, so next: 3 * 27 + 1 = 82")
(check-expect (collatz 82) "82 is even, so next: 82 / 2 = 41")
(check-expect (collatz 2) "2 is even, so next: 2 / 2 = 1")
(check-expect (collatz 1) "1 is 1, so done")

(define (collatz n)
  (string-append (number->string n) 
                 (cond 
                   [(= n 1) " is 1, so done"]
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
;; Given a number and a string, returns the next number in the Collatz sequence.
(check-expect (handle-key 27 "right") 82)
(check-expect (handle-key 82 "right") 41)
(check-expect (handle-key 2 "right") 1)

(define (handle-key state key)
  (cond
    [(string=? key "right") (next-collatz state)]
    [else state]))
    

;; render : Number -> Image
;; Given a number n, returns an image that displays the next number in the Collatz sequence.
(check-expect (render 27) (overlay
                           (rectangle 500 200 "outline" "lightblue")
                           (text "27 is odd, so next: 3 * 27 + 1 = 82" 12 "black")))
(check-expect (render 82) (overlay
                           (rectangle 500 200 "outline" "lightblue")
                           (text "82 is even, so next: 82 / 2 = 41" 12 "black")))

(define (render state)
  (overlay
   (rectangle 500 200 "outline" "lightblue")
   (text (collatz state) 12 "black")))


(big-bang 82
  (on-key handle-key)
  (to-draw render))
;; ====================================================