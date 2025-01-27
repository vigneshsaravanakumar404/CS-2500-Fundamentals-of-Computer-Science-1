;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P2-eclipse-skel) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; World programs & big bang
; Two types of programs:
; short-lived (or at least finite life), calculates a value
; long-lived, driven by user interaction--what we call "world programs"
;
; world programs have two new main ideas:
; - world state
; - event handlers
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Back to our motivating example: animating an eclipse
; What changes from moment to moment?
; - position of moon
; - color of sky
; - time
;
; consider each <BRAINSTORM>







; Let's base things on position of moon

; And what about our events? What do we want to react to?
; <BRAINSTORM>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; big-bang
;
; Vocabulary	big-bang
; Grammar	You call big-bang as follows:
;(big-bang initial-world-state
;  [to-draw to-draw-function-name] ; required
;  [event-type-1 handler1] ; optional
;  [event-type-2 handler2] ; optional
;  ...)
;
; Semantics	big-bang launches a world program. The initial world is initial-world-state.
;		It calls to-draw to render the world. It returns the last world state before exit.
;		Common events include...
;		on-tick		function that is repeatedly called whenever a particular amount
;				of time passes; produces the next world state
;
; 		on-key		function that is called whenever the user presses a key;
;				produces the next world state
;
;		stop-when	function called whenever the world state changes, returning
;				true means the program is over; can optionally supply another
;				function to show a final visualization
;
; NB: We cannot use check-expect on a function that uses big-bang, since we can't predict
; how the user will interact.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/image)
(require 2htdp/universe)
 
; RealNumber -> RealNumber
; Animates an eclipse
 
(define (eclipse initial-x)
  (big-bang initial-x
    [to-draw draw-eclipse]
    ; <ACTIVATE FOLLOWING ONE-BY-ONE:>
    ;[on-tick move-moon 0.1]
    ;[on-key restart-moon]
    ;[stop-when done-moon? the-end]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Design draw-eclipse

; draw-eclipse : RealNumber -> Image
; Draw the moon at the given x-coordinate, on a scene with the sun
 
(check-expect (draw-eclipse 0)
              (place-image MOON
                           0 (/ SKY-HEIGHT 2)
                           (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY)))

;
; We already have this from last time
;
(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)
 
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "black"))
 
(define (draw-eclipse x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN
                            (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            (cond
                              [(= x-moon (/ SKY-WIDTH 2)) DARKSKY]
                              [(< (abs (- x-moon (/ SKY-WIDTH 2))) (* 2 RADIUS)) DIMSKY]
                              [else SKY]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Design move-moon
; (first, go up and activate it in big-bang)

; move-moon : RealNumber -> RealNumber
; Moves the moon in the sky
;<DO NOW><SCROLL>








 
(check-expect (move-moon 5) 7)
(check-expect (move-moon 9) 11)
 
(define (move-moon x-moon)
  ...)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3: Design restart-moon : this is a key event handler
; (first, go up and activate it in big-bang)

; restart-moon : RealNumber KeyEvent -> RealNumber
; Moves the moon to the start
;<DO NOW><SCROLL>







 
(check-expect (restart-moon 5 " ") (- RADIUS))
(check-expect (restart-moon 32 "left") (- RADIUS))
 
(define (restart-moon x-moon ke)
  (- RADIUS))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 4: Design done-moon?
; (first, go up and activate it in big-bang)

; done-moon? : RealNumber -> Boolean
; Determines when the animation is over
;<DO NOW><SCROLL>







 
(check-expect (done-moon? 0) false)
(check-expect (done-moon? (+ SKY-WIDTH RADIUS)) true)
 
(define (done-moon? x-moon)
  ...)
 
; the-end : RealNumber -> Image
; Shows an ending screen
 
(check-expect (the-end 10) (overlay (text "The End" 20 "white")
                                    SKY))
 
(define (the-end x-moon)
  (overlay (text "The End" 20 "white")
           SKY))
