;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P1_define) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Apropos of nothing:
; Every programming language allows what are known as comments--
; note in a program that are meant for people to read, but not
; the computer. In Racket, the comment prefix is a ';' (semicolon).
; Anything on the rest of a line after the first semicolon is ignored
; (like the entirety of all of these first few lines :-)

;; From yesterday:
;  
; Vocabulary	define
; Grammar	(define NAME VALUE), where NAME can be any label.
;               By convention, make it uppercase (indicates a constant that does not change).
;               VALUE can be any value, or something that evaluates to a value.
; Semantics	Using define binds the name to that value, allowing you to use the name
;               elsewhere in the program.
;
; A few examples...

(define PI 3)
(define STUDENTS-IN-ROOM 114)
(define PEOPLE-IN-ROOM (+ 1 STUDENTS-IN-ROOM))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Try:
(require 2htdp/image)
#;
(place-image (circle 25 "solid" "yellow") 220 50 (rectangle 300 200 "solid" "light blue"))

; Better way:

(define SUN (circle 25 "solid" "yellow"))
(define SKY (rectangle 300 200 "solid" "light blue"))

; Now, let's try to put the sun lower and lower in the sky...

#;
(place-image SUN 220 50 SKY)
#;
(place-image SUN 220 60 SKY)
#;
(place-image SUN 220 70 SKY)
#;
(place-image SUN 220 80 SKY)
; A bit painful...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vocabulary	define, same as before.
; Grammar	(define (FUNCTION-NAME ARG-NAME ARG-NAME ...) VALUE),
;               where FUNCTION-NAME and ARG-NAME can be any label. As before,
;               VALUE can be any value, or something that evaluates to a value.
;               However, VALUE can use ARG-NAME inside of it.
; Semantics	As before, define binds the function name to that value,
;               allowing you to call that function elsewhere in your program.

; add-three : Number -> Number    ; <-- This is called a "signature"
; Adds three to the given number  ; <-- ... and this is called a "purpose statment"
(define (add-three x)
  (+ x 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now, for a function of multiple parameters:
; arguments are matched to parameters by position

; rectangle-area : Number Number -> Number
; Given the width and height of a rectangle, compute its area
(define (rectangle-area width height)
  (* width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Back to our astronomy lesson:
; 
; Exercise:
; Design a function draw-sun that accepts a y-coordinate, draws the sun at that y position.
 
; draw-sun : Number -> Image
; Draw the sun at the given y-coordinate
(define (draw-sun y)
  (place-image SUN 220 y SKY))

; What happens when we run...
;(draw-sun 10)
;(draw-sun 20)
;(draw-sun 30)

; Can we automate this? Yes, using "animate"
; First we need the universe set of functions (more later)...

(require 2htdp/universe)
; Now, simply run the line...

#;
(animate draw-sun)
