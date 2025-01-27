;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname eclipse-challenge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Load, then do:
;(draw-eclipse 50)




























(require 2htdp/image)
(require 2htdp/universe)



(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)
 
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
 
; draw-eclipse : Number -> Image
; Draw the moon at the given x-coordinate, on a scene with the sun

(define (draw-eclipse x-moon)
  (place-image
   MOON
   x-moon (/ SKY-HEIGHT 2)
   (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY)))
 
