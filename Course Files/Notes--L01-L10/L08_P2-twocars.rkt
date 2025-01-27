;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P3-twocars) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; PRACTICE 2: TWOCARS
; Suppose we want to create a world program that simulates two cars
; accelerating towards each other and colliding.
; What is the first step? What is the world? What changes? Let's make a list:
;<BRAINSTORM>








(define-struct twocars [x1 vx1 x2 vx2])

; Exercise:
; How many functions does that create? What are they?
;<DO NOW>








 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now use the make-twocars struct to do a data design for TwoCars:

; A TwoCars is...
;<DO NOW><SCROLL>








; A TwoCars is a (make-twocars Number Number Number Number)
; Interpretation: Two cars racing towards each other
;  - x1 is the x-position of the first car in pixels from the left
;  - vx1 is the x-velocity of the first car in pixels/tick (driving to the right)
;  - x2 is the x-position of the second car in pixels from the left
;  - vx2 is the x-velocity of the second car in pixels/tick (driving to the left)
 
(define TWOCARS-1 (make-twocars 1 2 300 4))
 
(define (twocars-temp tc)
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collision : TwoCars -> TwoCars
; Simulate a car crash
 
(define (collision initial-tc)
  (big-bang initial-tc
    [to-draw draw-twocars]
    [on-tick move-twocars]))
 
; draw-twocars : TwoCars -> Image
; Draws each of the cars
 
(define CAR-1 (rectangle 10 5 "solid" "blue"))
(define CAR-2 (rectangle 10 5 "solid" "red"))
(define Y-CAR 50)
(define BACKGROUND (empty-scene 600 400))
 
(check-expect (draw-twocars TWOCARS-1)
              (place-image CAR-1
                           1 Y-CAR
                           (place-image CAR-2 300 Y-CAR BACKGROUND)))
 
(define (draw-twocars tc)
  (place-image CAR-1
               (twocars-x1 tc)
               Y-CAR
               (place-image CAR-2 (twocars-x2 tc) Y-CAR BACKGROUND)))
 
 
; move-twocars : TwoCars -> TwoCars
; Moves each of the cars
 
(define ACCEL 0.1)
 
(check-expect (move-twocars TWOCARS-1)
              (make-twocars 3 2.1
                            296 4.1))
 
(define (move-twocars tc)
  (make-twocars (+ (twocars-x1 tc) (twocars-vx1 tc))
                (+ (twocars-vx1 tc) ACCEL)
                (- (twocars-x2 tc) (twocars-vx2 tc))
                (+ (twocars-vx2 tc) ACCEL)))
 
(collision (make-twocars 1 1 500 1))
