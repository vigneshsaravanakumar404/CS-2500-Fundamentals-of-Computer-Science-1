#lang htdp/bsl
(require 2htdp/universe)
(require 2htdp/image)

;; ====================================================
;; Exercise 2

; A MauriceSendakBook is one of ...
; - "Chicken Soup with Rice (1962)"
; - "Where the Wild Things Are (1963)"
; - "Higglety Pigglety Pop! (1967)"
; - "In the Night Kitchen (1970)"
; - "Seven Little Monsters (1977)"
; Interpretation: the name and year of books authored by Maurice Sendak

(define MauriceSendakBook1 "Chicken Soup with Rice (1962)")
(define MauriceSendakBook2 "Where the Wild Things Are (1963)")
(define MauriceSendakBook3 "Higglety Pigglety Pop! (1967)")

;; next-book : MauriceSendakBook -> MauriceSendakBook
;; Given MauriceSendakBook, returns the next book in the series. Newest book returns oldest book.
;; Example: (next-book (MauriceSendakBook1 "Chicken Soup with Rice (1962)")) => 
;;          (MauriceSendakBook5 "Seven Little Monsters (1977)")
(define (next-book b)
  (cond
    [(string=? b "Chicken Soup with Rice (1962)") "Where the Wild Things Are (1963)"]
    [(string=? b "Where the Wild Things Are (1963)") "Higglety Pigglety Pop! (1967)"]
    [(string=? b "Higglety Pigglety Pop! (1967)") "In the Night Kitchen (1970)"]
    [(string=? b "In the Night Kitchen (1970)") "Seven Little Monsters (1977)"]
    [else "Chicken Soup with Rice (1962)"]))


;; prev-book : MauriceSendakBook -> MauriceSendakBook
;; Given MauriceSendakBook, returns the previous book in the series. Oldest book returns newest book.
;; Example: (prev-book (MauriceSendakBook1 "Chicken Soup with Rice (1962)")) => 
;;          (MauriceSendakBook5 "Seven Little Monsters (1977)")
(define (prev-book b)
  (cond
    [(string=? b "Chicken Soup with Rice (1962)") "Seven Little Monsters (1977)"]
    [(string=? b "Where the Wild Things Are (1963)") "Chicken Soup with Rice (1962)"]
    [(string=? b "Higglety Pigglety Pop! (1967)") "Where the Wild Things Are (1963)"]
    [(string=? b "In the Night Kitchen (1970)") "Higglety Pigglety Pop! (1967)"]
    [else "In the Night Kitchen (1970)"]))

;; handle-key : MauriceSendakBook String -> MauriceSendakBook
;; Given a MauriceSendakBook and String, returns next/previous book based on the key pressed.
;; Example: (handle-key (MauriceSendakBook1 "Chicken Soup with Rice (1962)") "right") => 
;;          (MauriceSendakBook2 "Where the Wild Things Are (1963)")
(define (handle-key state key)
  (cond
    [(string=? key "right") (next-book state)]
    [(string=? key "left") (prev-book state)]
    [else state]))

;; render : MauriceSendakBook -> Image
;; Given a MauriceSendakBook, returns an image of the book.
;; Example: (render (MauriceSendakBook1 "Chicken Soup with Rice (1962)")) =>
;;          (text "Chicken Soup with Rice (1962)" 15 "black")
(define (render state)
  (overlay
   (rectangle 500 200 "outline" "white")
   (text state 32 "black")))

(big-bang "Chicken Soup with Rice (1962)"
  (on-key handle-key)
  (to-draw render))
;; ====================================================