#lang htdp/bsl
(require 2htdp/universe)
(require 2htdp/image)

;; Exercise 2

; A MauriceSendakBook is one of ...
; - "Chicken Soup with Rice (1962)"
; - "Where the Wild Things Are (1963)"
; - "Higglety Pigglety Pop! (1967)"
; - "In the Night Kitchen (1970)"
; - "Seven Little Monsters (1977)"
; Interpretation: the name and year of books authored by Maurice Sendak
(define CHICKEN-SOUP-WITH-RICE "Chicken Soup with Rice (1962)")
(define WHERE-THE-WILD-THINGS-ARE "Where the Wild Things Are (1963)")
(define HIGGLETY-PIGGLETY-POP "Higglety Pigglety Pop! (1967)")
(define IN-THE-NIGHT-KITCHEN "In the Night Kitchen (1970)")
(define SEVEN-LITTLE-MONSTERS "Seven Little Monsters (1977)")

(define (book-temp MauriceSendakBook)
  (...
   (cond
     [string=? MauriceSendakBook CHICKEN-SOUP-WITH-RICE] (... MauriceSendakBook))))

;; next-book : MauriceSendakBook -> MauriceSendakBook
;; Given MauriceSendakBook, returns the next book in the series. Newest book returns oldest book.
(check-expect (next-book CHICKEN-SOUP-WITH-RICE) WHERE-THE-WILD-THINGS-ARE)
(check-expect (next-book WHERE-THE-WILD-THINGS-ARE) HIGGLETY-PIGGLETY-POP)
(check-expect (next-book HIGGLETY-PIGGLETY-POP) IN-THE-NIGHT-KITCHEN)
(check-expect (next-book IN-THE-NIGHT-KITCHEN) SEVEN-LITTLE-MONSTERS)
(check-expect (next-book SEVEN-LITTLE-MONSTERS) CHICKEN-SOUP-WITH-RICE)

(define (next-book b)
  (cond
    [(string=? b CHICKEN-SOUP-WITH-RICE) WHERE-THE-WILD-THINGS-ARE]
    [(string=? b WHERE-THE-WILD-THINGS-ARE) HIGGLETY-PIGGLETY-POP]
    [(string=? b HIGGLETY-PIGGLETY-POP) IN-THE-NIGHT-KITCHEN]
    [(string=? b IN-THE-NIGHT-KITCHEN) SEVEN-LITTLE-MONSTERS]
    [else CHICKEN-SOUP-WITH-RICE]))


;; prev-book : MauriceSendakBook -> MauriceSendakBook
;; Given MauriceSendakBook, returns the previous book in the series. Oldest book returns newest book.
(check-expect (prev-book CHICKEN-SOUP-WITH-RICE) SEVEN-LITTLE-MONSTERS)
(check-expect (prev-book WHERE-THE-WILD-THINGS-ARE) CHICKEN-SOUP-WITH-RICE)
(check-expect (prev-book HIGGLETY-PIGGLETY-POP) WHERE-THE-WILD-THINGS-ARE)
(check-expect (prev-book IN-THE-NIGHT-KITCHEN) HIGGLETY-PIGGLETY-POP)
(check-expect (prev-book SEVEN-LITTLE-MONSTERS) IN-THE-NIGHT-KITCHEN)

(define (prev-book b)
  (cond
    [(string=? b CHICKEN-SOUP-WITH-RICE) SEVEN-LITTLE-MONSTERS]
    [(string=? b WHERE-THE-WILD-THINGS-ARE) CHICKEN-SOUP-WITH-RICE]
    [(string=? b HIGGLETY-PIGGLETY-POP) WHERE-THE-WILD-THINGS-ARE]
    [(string=? b IN-THE-NIGHT-KITCHEN) HIGGLETY-PIGGLETY-POP]
    [else "In the Night Kitchen (1970)"]))

;; handle-key : MauriceSendakBook String -> MauriceSendakBook
;; Given a MauriceSendakBook and String, returns next/previous book based on the key pressed.
(check-expect (handle-key CHICKEN-SOUP-WITH-RICE "right") WHERE-THE-WILD-THINGS-ARE)
(check-expect (handle-key WHERE-THE-WILD-THINGS-ARE "right") HIGGLETY-PIGGLETY-POP)
(check-expect (handle-key HIGGLETY-PIGGLETY-POP "right") IN-THE-NIGHT-KITCHEN)
(check-expect (handle-key IN-THE-NIGHT-KITCHEN "right") SEVEN-LITTLE-MONSTERS)
(check-expect (handle-key SEVEN-LITTLE-MONSTERS "right") CHICKEN-SOUP-WITH-RICE)

(define (handle-key state key)
  (cond
    [(string=? key "right") (next-book state)]
    [(string=? key "left") (prev-book state)]
    [else state]))

;; render : MauriceSendakBook -> Image
;; Given a MauriceSendakBook, returns an image of the book.
(define (render state)
  (overlay
   (rectangle 500 200 "outline" "white")
   (text state 32 "black")))

(big-bang "Chicken Soup with Rice (1962)"
  (on-key handle-key)
  (to-draw render))