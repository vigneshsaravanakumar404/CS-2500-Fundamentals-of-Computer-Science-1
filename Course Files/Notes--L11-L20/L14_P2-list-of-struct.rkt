;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L14_P2-list-of-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; USING LISTS OF STRUCTURES

; Recall our data definition for Position:

; A Position is a (make-posn Real Real)
; Interpretation: a 2d location
 
(define POSN-1 (make-posn 1 2))
(define POSN-2 (make-posn 5 6))

(define (posn-temp p)
  (... (posn-x p) ...
       (posn-y p) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Create a data definition for a list of Positions.


; A ListofPositions (LoP) is...
;<DO NOW><SCROLL>









; A ListofPositions (LoP) is one of:
; - '()
; - (cons Position LoP)
; Interpretation: a list of positions
 
(define LOP-1
  (cons POSN-1
        (cons POSN-2 '())))
 
(define (lop-temp lop)
  ;<DO NOW><SCROLL>







  
  (...
   (cond [(empty? lop) ...]
         [(cons? lop) (... (posn-temp (first lop)) ...
                           (lop-temp (rest lop)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function max-distance-from-origin that accepts a LoP and
; determines the maximal distance of any LoP from the origin.
; For this, we will need to use check-within as we are dealing
; with irrational numbers.


; max-distance-from-origin : LoP -> NonNegReal
; Finds the maximal distance of all positions from the origin

(check-expect (max-distance-from-origin '()) 0)

(check-within (max-distance-from-origin LOP-1)
              (sqrt 61) 0.001)

(check-within (max-distance-from-origin (cons POSN-1 '()))
              (sqrt 5) 0.001)
 
(define (max-distance-from-origin lop)
  ; Start from template
  #;
  (...
   (cond [(empty? lon) ...]
         [(cons? lon) (... (posn-temp (first lop)) ...
                           (lop-temp (rest lop)) ...)]))
  ;<DO NOW><SCROLL-RM> helper: distance-from-origin






  
  (cond [(empty? lop) 0]
        [(cons? lop) (max (distance-from-origin (first lop))
                          (max-distance-from-origin (rest lop)))]))
 
; distance-from-origin : Position -> NonNegReal
; Calculates the distance to the origin of the given position
 
(check-within (distance-from-origin POSN-1) (sqrt 5) 0.001)
(check-within (distance-from-origin POSN-2) (sqrt 61) 0.001)
 
(define (distance-from-origin p)
  (sqrt (+ (sqr (posn-x p))
           (sqr (posn-y p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/universe)
(require 2htdp/image)

;; A WorldState is one of:
;; - '()
;; - (cons Posn WorldState)

;; mouse-event-handler : WorldState Number Number MouseEvent -> WorldState
(define (mouse-event-handler ws x y event)
  (cond
    [(string=? event "button-down") (cons (make-posn x y) ws)]
    [else ws]))

;; draw-circles : WorldState -> Image
(define (draw-circles ws)
  (cond
    [(empty? ws) (square 400 "solid" "black")]
    [(cons? ws)
     (place-image (circle 10 "solid" "red")
                  (posn-x (first ws))
                  (posn-y (first ws))
                  (draw-circles (rest ws)))]))

#;
(big-bang '()
  [to-draw draw-circles]
  [on-mouse mouse-event-handler])