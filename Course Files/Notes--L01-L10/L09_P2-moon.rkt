;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L10_P2-moon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Exercise:
; Design a World program that waits until the user presses a key, then moves the moon
; across the night sky, and finally, when it reaches the end, displays that it’s done

; Step 1: World state data design:
; We have three sequential states to model: before, during, and after the movement.
; Is this an enumeration, or a union?
; <BRAINSTORM>
; A MoonPosition (MP) is...








; A MoonPosition (MP) is one of:
; - "waiting"
; - Number
; - #true
; Interpretation: we are waiting for a key
; to be pressed, the location of the
; moon on the x-axis, or that the moon
; has traversed the sky
 
(define MP-W "waiting")
(define MP-1 1)
(define MP-T #true)
 
(define (mp-temp mp)
  (...
   (cond
     [(string? mp) ...]
     [(number? mp) ... mp ...]
     [(boolean? mp) ...])))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 2: design our main World function, so that we know what our
; wishlist is:

; moon-run : MP -> MP
; Animates a moon
 
(define (moon-run initial-mp)
  (big-bang initial-mp
    ; What event handlers will we need?
    ;<DO NOW><SCROLL>







    
    [to-draw draw-mp]
    [on-tick move-mp]
    [on-key start-mp]))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 3: design the functions in our wishlist

; draw-mp : MP -> Image
; Visualizes a moon animation
 
(define MP-SIZE 600)
(define MP-HALF (/ MP-SIZE 2))
(define MP-RADIUS (* MP-SIZE 0.1))
 
(define BG (square MP-SIZE "solid" "black"))
(define MOON (circle MP-RADIUS "solid" "gray"))
 
(check-expect (draw-mp MP-W) (overlay (text "Press a Key" 20 "white")
                                      BG))
 
(check-expect (draw-mp MP-1) (place-image MOON
                                          1 MP-HALF
                                          BG))
 
(check-expect (draw-mp MP-T) (overlay (text "Done!" 20 "white")
                                      BG))
 
(define (draw-mp mp)
  ;<DO NOW><SCROLL-RM> Start with what?







  
  (cond
    [(string? mp) (overlay (text "Press a Key" 20 "white") BG)]
    [(number? mp) (place-image MOON mp MP-HALF BG)]
    [(boolean? mp) (overlay (text "Done!" 20 "white") BG)]))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move-mp : MP -> MP
; Moves a started moon-position to the next stage
 
(check-expect (move-mp MP-W) MP-W)
(check-expect (move-mp MP-1) 2)
(check-expect (move-mp (+ MP-SIZE MP-RADIUS)) MP-T)
(check-expect (move-mp MP-T) MP-T)
 
(define (move-mp mp)
  ;<DO NOW><SCROLL-RM> Start with what?







  
  (cond
    [(string? mp) mp]
    [(number? mp)
     (if (< mp (+ MP-SIZE MP-RADIUS))
         (add1 mp)
         MP-T)]
    [(boolean? mp) mp]))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start-mp : MP KeyEvent -> MP
; Starts a waiting MP on the left of the screen
 
(check-expect (start-mp MP-W " ") (- MP-RADIUS))
(check-expect (start-mp MP-W "t") (- MP-RADIUS))
(check-expect (start-mp MP-1 "right") MP-1)
(check-expect (start-mp MP-T " ") MP-T)
 
(define (start-mp mp ke)
  ;<DO NOW><SCROLL-RM>







  
  (cond
    [(string? mp) (- MP-RADIUS)]
    [(number? mp) mp]
    [(boolean? mp) mp]))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;
(moon-run MP-W)
