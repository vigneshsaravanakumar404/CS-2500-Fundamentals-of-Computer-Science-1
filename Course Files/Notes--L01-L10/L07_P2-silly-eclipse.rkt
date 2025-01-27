;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L07_P2-silly-eclipse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the world program eclipse that shows an eclipse where the moon moves randomly up/down
; and left/right.

; Quick aside about randomness: can computers produce truly random numbers?
; <DISCUSS>

; In BSL: the random function gives us an integer in [0, n). How could you use this to create
; a random motion to one of the eight adjacent positions, or possibly stay still?

; First, create a data definition for representing our moon's 2-D position:

; A MoonPosition is...
;<DO NOW><SCROLL>








; A MoonPosition is a (make-posn Integer Integer)
; Represents the moon's 2D location
 
(define POSN-0 (make-posn 0 0))
(define POSN-1 (make-posn -5 3))
(define POSN-2 (make-posn 50 50))
 
(define (moonposition-temp p)
  (... (posn-x p) ...
       (posn-y p) ...))
 
; silly-eclipse : MoonPosition -> MoonPosition
; Visualizes the moon moving randomly in the sky
 
(define (silly-eclipse initial-position)
  (big-bang initial-position
    [to-draw draw-eclipse]
    [on-tick move-moon]))
 
; draw-eclipse : MoonPosition -> Image
; Visualizes the moon in the sky
 
(define SKY (square 100 "solid" "light blue"))
(define SUN (circle 10 "solid" "yellow"))
(define MOON (circle 10 "solid" "gray"))
 
(define BACKGROUND (overlay SUN SKY))

; The check-expect and code need to be updated:

; ORIG:
;(check-expect (draw-eclipse 10)
;              (place-image MOON 10 (/ SKY-HEIGHT 2) BACKGROUND))
; 
;(define (draw-eclipse x-moon)
;  (place-image MOON
;               x-moon (/ SKY-HEIGHT 2)
;               BACKGROUND))

; UPDATED:
(check-expect (draw-eclipse (make-posn 10 20))
              (place-image MOON 10 20 BACKGROUND))
 
(define (draw-eclipse p)
  (place-image MOON
               (posn-x p) (posn-y p)
               BACKGROUND))
 
; move-moon : MoonPosition -> MoonPosition
; Moves the moon a random amount in [-1, 1] in both x and y

; How do we check-expect randomly generated values??

(check-random
 (move-moon POSN-2)
 (make-posn (+ (sub1 (random 3)) (posn-x POSN-2))
            (+ (sub1 (random 3)) (posn-y POSN-2))))
 
(define (move-moon p)
  (make-posn (+ (sub1 (random 3)) (posn-x p))
             (+ (sub1 (random 3)) (posn-y p))))
 
(define POSN-2p (move-moon POSN-2))

; Another novel application of check-expect
;<DISCUSS THIS: BOTH boolean expr AND define ABOVE>
(check-expect
 (and (> (posn-x POSN-2p) 48)
      (< (posn-x POSN-2p) 52)
      (> (posn-y POSN-2p) 48)
      (< (posn-x POSN-2p) 52))
 #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Suppose we wanted to make the sun move as well, independent of the moon.
; How could we upgrade the world-state to support that?
;<BRAINSTORM>








; One idea might be a nested structure like...

; A SunMoonPosition is a (make-posn (make-posn Number Number) (make-posn Number Number))
; Interpretation:  The location of the moon and the sun on the x- and y-axis
 
(define SMP-1 (make-posn (make-posn 3 4) (make-posn 5 5)))
 
(define (sunmoonposition-temp smp)
  (... (posn-x (posn-x smp)) ... (posn-y (posn-x smp)) ...
       (posn-x (posn-y smp)) ... (posn-y (posn-y smp)) ...))

; What is wrong with this?
