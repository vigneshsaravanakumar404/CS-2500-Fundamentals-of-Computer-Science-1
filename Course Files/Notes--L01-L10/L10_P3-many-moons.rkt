;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L10_P3-many-moons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; TOWARDS ARBITRARILY LARGE DATA

; Let's go back and take a closer look at the code we wrote for the collision simulator,
; specifically at the code moving/drawing two cars...

; <Examine draw-twocars and move-twocars>

; There is unnecessary duplication in this code. Imagine trying to model
; three or four cars.

; Recall the T/F test example from last week. Think of how tedious it would have
; been if we had had the TFTest structure itself have a q1-prompt and q1-answer
; field, then a q2-prompt and q2-answer field, and so on. For the TFTest, we
; instead applied a strategy of nested structured data--structure data
; within/as-part-of larger structured data (in our case, TFQuestion structures were
; nested inside of a TFTest structure). Let's apply that same principle to
; a multi-moon simulation:

(require 2htdp/image)
(require 2htdp/universe)

(define-struct moon [x vx])
 
; A Moon is a (make-moon Number Number)
; Representing the position and velocity of a moon.
;  - x is the x-position of the moon
;  - vx is the x-velocity of the moon
 
(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 300 -1))
 
(define (moon-temp m)
  (... (moon-x m) ...
       (moon-vx m) ...))
 
 
(define-struct twomoons [m1 m2])
 
; A TwoMoons is a (make-twomoons Moon Moon)
; Representing the two moons we're drawing.
;  - m1 is the first moon
;  - m2 is the second moon
 
(define TWOMOONS-1 (make-twomoons MOON-1 MOON-2))
 
(define (twomoons-temp tm)
  (... (moon-temp (twomoons-m1 tm))  ...
       (moon-temp (twomoons-m2 tm)) ...))


; eclipse : TwoMoons -> TwoMoons
; Runs an eclipse of two moons
 
(define (eclipse initial-tm)
  (big-bang initial-tm
    [to-draw draw-eclipse]
    [on-tick move-eclipse]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move-eclipse : TwoMoons -> TwoMoons
; Moves two moons for one tick
 
(check-expect (move-eclipse TWOMOONS-1)
              (make-twomoons (make-moon 11 1)
                             (make-moon 299 -1)))
 
(define (move-eclipse tm)
  ; Go grab the template and get going!
  ;<DO NOW><SCROLL-RM>








  (make-twomoons (move-moon (twomoons-m1 tm))
                 (move-moon (twomoons-m2 tm))))
 
; move-moon : Moon -> Moon
; Moves a single moon
 
(check-expect (move-moon MOON-1)
              (make-moon 11 1))
 
(check-expect (move-moon MOON-2)
              (make-moon 299 -1))
 
(define (move-moon m)
  ; Start w/template...
  ;<DO NOW><SCROLL-RM>








  (make-moon (+ (moon-x m) (moon-vx m))
             (moon-vx m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; draw-eclipse : TwoMoons -> Image
; Draw the moons on a scene with the sun
 
(define SIZE 400)
(define HALF (/ SIZE 2))
 
(define SUN (circle (/ SIZE 10) "solid" "yellow"))
(define MOON (circle (/ SIZE 10) "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))
 
(check-expect (draw-eclipse TWOMOONS-1)
              (place-image MOON
                           10 HALF
                           (place-image MOON
                                        300 HALF
                                        (place-image SUN
                                                     HALF HALF
                                                     SKY))))
 
(define (draw-eclipse tm)
  ; Start w/template...
  ; (Think about this one--it's a little bit tricky!)
  ;<DO NOW><SCROLL-RM>








  (draw-moon (twomoons-m1 tm)
             (draw-moon (twomoons-m2 tm)
                        (place-image SUN HALF HALF SKY))))
 
; draw-moon : Moon Image -> Image
; Draws a moon onto a background
 
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) HALF SKY))
 
(define (draw-moon moon background)
  ; Start w/template...
  ;<DO NOW><SCROLL-RM>








  (place-image MOON
               (moon-x moon) HALF
               background))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now let's suppose that you're on Jupiter. How many moons does Jupiter have?

; An EightyMoons is a (make-twomoons Moon Moon Moon Moon ...)
; Representing the 79... no, 80(!) moons we're drawing.
