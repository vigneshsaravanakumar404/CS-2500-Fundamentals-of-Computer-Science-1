;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L15_P3-list-of-moons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
  (... (moon-x m) ... (moon-vx m) ...))
 
; A ListofMoons (LoM) is one of:
; - '()
; - (cons Moon LoM)
; Interpretation: a list of moons

(define LOM-0 '())
(define LOM-1 (cons MOON-1 LOM-0))
(define LOM-2 (cons MOON-2 LOM-1))

(define (lom-temp lom)
  (...
   (cond [(empty? lom) ...]
         [(cons? lom) (... (moon-temp (first lom)) ...
                           (lom-temp (rest lom)) ...)])))
 
; eclipse : LoM -> LoM
; Runs an eclipse of moons
 
(define (eclipse initial-lom)
  (big-bang initial-lom
    [to-draw draw-eclipse]
    [on-tick move-eclipse]
    [on-key add-moon]))
 
; draw-eclipse : LoM -> Image
; Draw the moons on a scene with the sun
 
(define SIZE 400)
(define HALF (/ SIZE 2))
(define RADIUS (/ SIZE 10))
 
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))
 
(check-expect (draw-eclipse LOM-0)
              (place-image
               SUN
               HALF HALF
               SKY))
 
(check-expect (draw-eclipse LOM-2)
              (place-image
               MOON
               300 HALF
               (place-image
                MOON
                10 HALF
                (place-image
                 SUN
                 HALF HALF
                 SKY))))
 
(define (draw-eclipse lom)
  (cond [(empty? lom) (place-image SUN HALF HALF SKY)]
        [(cons? lom) (draw-moon (first lom)
                                (draw-eclipse (rest lom)))]))
 
; draw-moon : Moon Image -> Image
; Draws a moon onto a background
 
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) HALF SKY))
 
(define (draw-moon moon background)
  (place-image MOON
               (moon-x moon) HALF
               background))
 
; move-eclipse : LoM -> LoM
; Moves moons for one tick
 
(check-expect (move-eclipse LOM-0) LOM-0)
 
(check-expect (move-eclipse LOM-2)
              (cons
               (make-moon 299 -1)
               (cons
                (make-moon 11 1)
                '())))
 
(define (move-eclipse lom)
  (cond [(empty? lom) '()]
        [(cons? lom) (cons (move-moon (first lom))
                           (move-eclipse (rest lom)))]))
 
; move-moon : Moon -> Moon
; Moves a single moon
 
(check-expect (move-moon MOON-1)
              (make-moon 11 1))
 
(check-expect (move-moon MOON-2)
              (make-moon 299 -1))
 
(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-vx m))
             (moon-vx m)))
 
; add-moon : LoM KeyEvent -> LoM
; Adds a moon to the system
 
(define NEW-MOON-X 1)
(define NEW-MOON-VX 20)
 
(check-expect (add-moon LOM-0 " ")
              (cons (make-moon NEW-MOON-X NEW-MOON-VX) '()))
 
(check-expect (add-moon LOM-2 "right")
              (cons (make-moon NEW-MOON-X NEW-MOON-VX) LOM-2))
 
(define (add-moon lom ke)
  (cons (make-moon NEW-MOON-X NEW-MOON-VX) lom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; REMOVING OLD MOONS

; Leaving the moons drifting off the right edge will cause the list of moons
; to grow endlessly, which can eventually slow down a computer. Let's cull those
; that have moved beyond the right border of the frame.
;<BRAINSTORM><SCROLL>







; First move, then filter those out of sight.
; Start with big-bang, then design move-and-filter-eclipse -> filter-eclipse -> keep-moon?.


; move-and-filter-eclipse : LoM -> LoM
; Moves moons for one tick, filtering those off the screen
 
(check-expect (move-and-filter-eclipse LOM-0) LOM-0)
 
(check-expect (move-and-filter-eclipse LOM-2)
              (cons (make-moon 299 -1)
                    (cons (make-moon 11 1)
                          '())))
 
(check-expect (move-and-filter-eclipse (cons (make-moon (* 2 SIZE) 1)
                                             (cons (make-moon 10 1)
                                                   '())))
              (cons (make-moon 11 1) '()))
 
(define (move-and-filter-eclipse lom)
  ; Start from template
  #;
  (...
   (cond [(empty? lom) ...]
         [(cons? lom) (... (moon-temp (first lom)) ...
                           (lom-temp (rest lom)) ...)]))
  ;<DO NOW><SCROLL-RM> helpers: move-eclipse, filter-eclipse




  
  (filter-eclipse
   (move-eclipse lom)))

; filter-eclipse : LoM -> LoM
; Filters those moons off the screen
 
(check-expect (filter-eclipse (cons (make-moon (* 2 SIZE) 1)
                                    (cons (make-moon 10 1) '())))
              (cons (make-moon 10 1) '()))
 
(define (filter-eclipse lom)
  ; Start from template
  #;
  (...
   (cond [(empty? lom) ...]
         [(cons? lom) (... (moon-temp (first lom)) ...
                           (lom-temp (rest lom)) ...)]))
  ;<DO NOW><SCROLL-RM> helpers: keep-moon?



  
  (cond [(empty? lom) '()]
        [(cons? lom) (if (keep-moon? (first lom))
                         (cons (first lom) (filter-eclipse (rest lom)))
                         (filter-eclipse (rest lom)))]))

; keep-moon? : Moon -> Boolean
; Determines if a moon is visible
 
(check-expect (keep-moon? (make-moon (* 2 SIZE) 1)) #false)
(check-expect (keep-moon? (make-moon (* 0.5 SIZE) 1)) #true)
 
(define (keep-moon? m)
  (< (moon-x m) (+ SIZE RADIUS)))

; Lastly, don't forget to update event handler in big-bang call
;<DO NOW>
