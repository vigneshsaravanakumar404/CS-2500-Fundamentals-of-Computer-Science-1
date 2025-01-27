;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L06_P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check-expect of images
; check-expect of complex results, and reusing code from body
; why no "(if <expr> #true #false)"?
; Review big-bang eclipse
; Are there any bugs? (Hint: yes!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise: draw a "traffic light" (actually, just a dot that
; changes color in the usual pattern)

; A TrafficLight is...
;<DO NOW><SCROLL>








; A TrafficLight is one of:
; - "red"
; - "yellow"
; - "green"
; and represents the colors of a traffic light
 
(define TRAFFICLIGHT-RED "red")
(define TRAFFICLIGHT-YELLOW "yellow")
(define TRAFFICLIGHT-GREEN "green")
 
(define (trafficlight-temp tl)
  (...
   ; <DO NOW><SCROLL>







   
   (cond
     [(string=? tl TRAFFICLIGHT-RED) ...]
     [(string=? tl TRAFFICLIGHT-YELLOW) ...]
     [(string=? tl TRAFFICLIGHT-GREEN) ...])))

; Now, the actual animation

; loop-light : TrafficLight -> TrafficLight
; Visualizes a looping traffic light
 
(define (loop-light tl)
  (big-bang tl
    ; What event handlers will we need? <DO NOW><SCROLL>








    [to-draw draw-tl]
    [on-tick next-tl 1]))


; Exercise: design the function draw-tl that draws a traffic light in the
; current state
;<DO NOW><SCROLL>








; draw-tl : TrafficLight -> Image
; Visualizes a traffic light
 
(define TL-RADIUS 50)
 
(check-expect (draw-tl TRAFFICLIGHT-RED)
              (circle TL-RADIUS "solid" "red"))
 
(check-expect (draw-tl TRAFFICLIGHT-YELLOW)
              (circle TL-RADIUS "solid" "yellow"))
 
(check-expect (draw-tl TRAFFICLIGHT-GREEN)
              (circle TL-RADIUS "solid" "green"))

(define (draw-tl-v1 tl)
  (cond
    [(string=? tl TRAFFICLIGHT-RED) (circle TL-RADIUS "solid" "red")]
    [(string=? tl TRAFFICLIGHT-YELLOW) (circle TL-RADIUS "solid" "yellow")]
    [(string=? tl TRAFFICLIGHT-GREEN) (circle TL-RADIUS "solid" "green")]))

; next-tl : TrafficLight -> TrafficLight
; Loops the light
 
(check-expect (next-tl TRAFFICLIGHT-RED) TRAFFICLIGHT-GREEN)
(check-expect (next-tl TRAFFICLIGHT-YELLOW) TRAFFICLIGHT-RED)
(check-expect (next-tl TRAFFICLIGHT-GREEN) TRAFFICLIGHT-YELLOW)
 
(define (next-tl tl)
  (cond
    [(string=? tl TRAFFICLIGHT-RED) TRAFFICLIGHT-GREEN]
    [(string=? tl TRAFFICLIGHT-YELLOW) TRAFFICLIGHT-RED]
    [(string=? tl TRAFFICLIGHT-GREEN) TRAFFICLIGHT-YELLOW]))

#;(loop-light TRAFFICLIGHT-GREEN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RealNumber -> RealNumber
; Animates an eclipse

(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "black"))
 
(define (eclipse initial-x)
  (big-bang initial-x
    [to-draw draw-eclipse]
    [on-tick move-moon 0.1]
    [on-key restart-moon]
    [stop-when done-moon? the-end]))

; draw-eclipse : RealNumber -> Image
; Draw the moon at the given x-coordinate, on a scene with the sun
(check-expect (draw-eclipse 0)
              (place-image MOON
                           0 (/ SKY-HEIGHT 2)
                           (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY)))

(define (draw-eclipse x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN
                            (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            (cond
                              [(= x-moon (/ SKY-WIDTH 2)) DARKSKY]
                              [(< (abs (- x-moon (/ SKY-WIDTH 2))) (* 2 RADIUS)) DIMSKY]
                              [else SKY]))))

; move-moon : RealNumber -> RealNumber
; Moves the moon in the sky
(check-expect (move-moon 5) 7)
(check-expect (move-moon 9) 11)
 
(define (move-moon x-moon)
  (+ 2 x-moon))


; Exercise:
; Design the function restart-moon that resets the moon position to the left border
; <DO NOW><SCROLL>








; restart-moon : RealNumber KeyEvent -> RealNumber
; Moves the moon to the start
(check-expect (restart-moon 5 " ") (- RADIUS))
(check-expect (restart-moon 32 "left") (- RADIUS))
 
(define (restart-moon x-moon ke)
  (- RADIUS))
 
; done-moon? : RealNumber -> Boolean
; Determines when the animation is over
(check-expect (done-moon? 0) false)
(check-expect (done-moon? (+ SKY-WIDTH RADIUS)) true)
 
(define (done-moon? x-moon)
  (= x-moon (+ SKY-WIDTH RADIUS)))
 
; the-end : RealNumber -> Image
; Shows an ending screen
(check-expect (the-end 10) (overlay (text "The End" 20 "white")
                                    SKY))
 
(define (the-end x-moon)
  (overlay (text "The End" 20 "white")
           SKY))
