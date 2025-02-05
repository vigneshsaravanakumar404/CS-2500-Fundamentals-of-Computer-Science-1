;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L10_P3-twocars) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Let's finish our collision simulation from the previous lecture.
(require 2htdp/image)
(require 2htdp/universe)

; Start by reviewing the old code (L08/P2-twocars.rkt)

; What was missing? The cars pass right through each other, and there is no indication
; that a collision took place. What do we have to upgrade to support this? The
; data design as well as the function designs, likely.

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's start with the data design. Our world is now "modal"--in the beginning,
; we have two cars, but after the collision, we have a wreck.
; Think back to the moon example...
;<BRAINSTORM>








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Update our data definition to represent a wreck as well.
; The original code for TwoCars is a good starting point:

(define-struct twocars [x1 vx1 x2 vx2])
 
; A TwoCars is a (make-twocars Number Number Number Number)
; Interpretation: Two cars racing towards each other
;  - x1 is the x-position of the first car in pixels from the left
;  - vx1 is the x-velocity of the first car in pixels/tick (driving to the right)
;  - x2 is the x-position of the second car in pixels from the left
;  - vx2 is the x-velocity of the second car in pixels/tick (driving to the left)
 
(define TWOCARS-1 (make-twocars 1 2 300 4))
(define TWOCARS-2 (make-twocars 8 2 12 4))
 
(define (twocars-temp tc)
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...))

; We need to add a data design for a wreck
;<DO NOW><SCROLL>








(define-struct wreck [x vx])

; A Wreck is a (make-wreck Number Number)
; - x is the x-position of the wreck in pixels from the left
; - vx is the x-velocity of the wreck in pixels/tick (driving to the right)
; Interpretation: the combined "car" after two cars hit
 
(define WRECK-1 (make-wreck 10 -1))  ; ERR: vx was -2
 
(define (wreck-temp w)
  (... (wreck-x w) ...
       (wreck-vx w) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; To paraphrase Lord of the Rings, we need "one data design to rule them all,
; and in the darkness bind them":

; A CollisionSim (CS) is... ???
;<DO NOW><SCROLL-RM>








; A CollisionSim (CS) is one of:
; - TwoCars
; - Wreck
; Interpretation: either two cars racing towards each other,
; or the combined car after they've hit
 
(define CS-1 TWOCARS-1)
(define CS-2 WRECK-1)
 
(define (cs-temp cs)
  (...
   (cond
     [(twocars? cs) (twocars-temp cs)]
     [(wreck? cs) (wreck-temp cs)])))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now that we have our data designs, let's upgrade the functions.

; The main function collision seems like it doesn't need to change,
; but actually the design does need to be tweaked to account for our
; new World state data type CS (in fact, we should make sure we consistently
; update this in all our function that take or return the World state)

; collision : CS -> CS
; Simulate a car crash
 
(define (collision initial-cs)
  (big-bang initial-cs
    [to-draw draw-cs]
    [on-tick move-cs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Update our drawing function. What it does (including the tests)
; is a superset of what it used to do; since our World state is a
; union, that means examples are actually either a TwoCars, which we
; already handle and test, or a Wreck, which we need to add.

; draw-cs : CS -> Image
; Draws a collision simulation
 
(define CAR-1 (rectangle 10 5 "solid" "blue"))
(define CAR-2 (rectangle 10 5 "solid" "red"))

; NEW:
(define CAR-W (rectangle 10 5 "solid" "purple"))

(define Y-CAR 50)
(define BACKGROUND (empty-scene 600 400))

(check-expect (draw-cs TWOCARS-1)
              (place-image CAR-1
                           1 Y-CAR
                           (place-image CAR-2 300 Y-CAR BACKGROUND)))
 
; NEW:
(check-expect (draw-cs WRECK-1)
              (place-image CAR-W
                           10 Y-CAR
                           BACKGROUND))
 
(define (draw-cs cs)
  #; ; start with the template:
  (...
   (cond
     [(twocars? cs) (twocars-temp cs)]
     [(wreck? cs) (wreck-temp cs)]))
  ;<DO NOW><SCROLL-RM>







  
  (cond
    [(twocars? cs) (draw-twocars cs)]
    [(wreck? cs) (draw-wreck cs)]))


; This is the exact same design as before, even the tests
; draw-twocars : TwoCars -> Image
; Draws each of the cars
 
(check-expect (draw-twocars TWOCARS-1)
              (place-image CAR-1
                           1 Y-CAR
                           (place-image CAR-2 300 Y-CAR BACKGROUND)))
 
(define (draw-twocars tc)
  (place-image CAR-1
               (twocars-x1 tc)
               Y-CAR
               (place-image CAR-2 (twocars-x2 tc) Y-CAR BACKGROUND)))


; NEW:
; draw-wreck : Wreck -> Image
; Draws the wreck
 
(check-expect (draw-wreck WRECK-1)
              (place-image CAR-W
                           10 Y-CAR
                           BACKGROUND))
 
(define (draw-wreck w)
  (place-image CAR-W
               (wreck-x w) Y-CAR
               BACKGROUND))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Next, update our move function. Similar to the drawing functions, this
; is an addition to what we used to do, with handling for Wrecks added

; move-cs : CS -> CS
; Moves the simulation
 
(define ACCEL 0.1)
 
(check-expect (move-cs TWOCARS-1)
              (make-twocars 3 2.1
                            296 4.1))

(check-expect (move-cs WRECK-1)
              (make-wreck 9 -1))  ; ERR: x/vx was 8,-2
 
(define (move-cs cs)
  #; ; start with the template:
  (...
   (cond
     [(twocars? cs) (twocars-temp cs)]
     [(wreck? cs) (wreck-temp cs)]))
  ;<DO NOW><SCROLL-RM>







  
  (cond
    [(twocars? cs) (move-twocars cs)]
    [(wreck? cs) (move-wreck cs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the exact same design as before, even the tests
; move-twocars : TwoCars -> TwoCars
; Moves each of the cars
 
(check-expect (move-twocars TWOCARS-1) (make-twocars 3 2.1 296 4.1))
 
(define (move-twocars tc)
  (make-twocars (+ (twocars-x1 tc) (twocars-vx1 tc))
                (+ (twocars-vx1 tc) ACCEL)
                (- (twocars-x2 tc) (twocars-vx2 tc))
                (+ (twocars-vx2 tc) ACCEL)))


;NEW:
; move-wreck : Wreck -> Wreck
; Moves the wreck
 
(check-expect (move-wreck WRECK-1) (make-wreck 9 -1))  ; ERR: was 8,-2
 
(define (move-wreck w)
  (make-wreck (+ (wreck-x w) (wreck-vx w))
              (wreck-vx w)))

; Whew--I think we're done! Let's try running it 
#;(collision (make-twocars 1 1 500 2))

; Hmm...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What went wrong?

(define (collision/v2 initial-cs)
  (big-bang initial-cs
    [to-draw draw-cs]
    [on-tick move-cs/v2]))

(check-expect (move-cs/v2 TWOCARS-2)
              WRECK-1)
 
(define (move-cs/v2 cs)
  ; old version:
  #;
  (cond
    [(twocars? cs) (move-twocars cs)]
    [(wreck? cs) (move-wreck cs)])
  ; Fix it to detect/handle the moment of collision
  ;<DO NOW><SCROLL-RM> <helpers collided?, tc->wreck>







  
  (cond
    [(twocars? cs) (if (collided? cs)
                       (tc->wreck cs)
                       (move-twocars cs))]
    [(wreck? cs) (move-wreck cs)]))

; collided? : ??? -> ???
; Determines if the cars collide this time step
 
(check-expect (collided? TWOCARS-1) false)
(check-expect (collided? TWOCARS-2) true)
 
(define (collided? tc)
  #; ; start with the template:
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...)
  ;<DO NOW><SCROLL-RM>







  
  (>= (+ (twocars-x1 tc) (twocars-vx1 tc))
      (- (twocars-x2 tc) (twocars-vx2 tc))))


; tc->wreck : ??? -> ???
; Converts a two-car situation into a wreck
 
(check-expect (tc->wreck TWOCARS-2) WRECK-1)
 
(define (tc->wreck tc)
  #; ; start with the template:
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...)
  ;<DO NOW><SCROLL-RM>







  
  (make-wreck
   (/ (+ (twocars-x1 tc) (twocars-x2 tc)) 2)
   (/ (- (twocars-vx1 tc) (twocars-vx2 tc)) 2)))


; Let's try running it again
#;(collision/v2 (make-twocars 1 1 500 2))
