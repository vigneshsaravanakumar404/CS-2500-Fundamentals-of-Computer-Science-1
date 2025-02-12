;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;                              Exercise 1
;; ======================================================================

;; A Material is one of:
; - "gold"
; - "silver"
; - "pewter"

(define MATERIAL-1 "gold")
(define MATERIAL-2 "silver")
(define MATERIAL-3 "pewter")

(define-struct charm [description material])

; A Charm is a (make-charm String Material)
; Interpretation: A charm with a description and material
;  - Description is the description of the charm
;  - Material is the material of the charm

; make-charm : String Material -> Charm
; charm? : Any -> Boolean
; charm-description : Charm -> String
; charm-material : Charm -> Material


(define CHARM-1 (make-charm "heart" MATERIAL-1))
(define CHARM-2 (make-charm "star" MATERIAL-2))
(define CHARM-3 (make-charm "moon" MATERIAL-3))

(define (charm-temp t)
  (... (charm-description t) ...
       (charm-material t) ...))


(define-struct charmBracelet [charm charmBracelet])

; A CharmBracelet is one of:
; - #false
; - (make-charmBracelet Charm CharmBracelet)
; Interpretation: A charm bracelet with a charm and a charm bracelet
;  - Charm is the charm on the bracelet
;  - CharmBracelet is the rest of the bracelet

; make-charmBracelet : Charm CharmBracelet -> CharmBracelet
; charmBracelet? : Any -> Boolean
; charmBracelet-charm : CharmBracelet -> Charm
; charmBracelet-charmBracelet : CharmBracelet -> CharmBracelet

(define CHARM-BRACELET-0 #false)
(define CHARM-BRACELET-1 (make-charmBracelet CHARM-1 CHARM-BRACELET-0))
(define CHARM-BRACELET-2 (make-charmBracelet CHARM-2 CHARM-BRACELET-1))
(define CHARM-BRACELET-3 (make-charmBracelet CHARM-3 CHARM-BRACELET-2))

(define (charmBracelet-temp t)
  (cond
    [(charmBracelet? t)
     (... (charm-temp (charmBracelet-charm t)) ...
          (charmBracelet-temp (charmBracelet-charmBracelet t)) ...)]
    [else
     (...)]))


; material-cost: Charm -> Number
; returns the cost of material
(check-expect (material-cost #false) 0)
(check-expect (material-cost CHARM-1) 15)
(check-expect (material-cost CHARM-2) 12)
(check-expect (material-cost CHARM-3) 10)

(define (material-cost c)
  (cond
    [(boolean? c) 0]
    [(string=? "gold" (charm-material c)) 15]
    [(string=? "silver" (charm-material c)) 12]
    [(string=? "pewter" (charm-material c)) 10]))

; bracelet-cost: CharmBracelet -> Number
; returns the cost of the bracelet
(check-expect (bracelet-cost CHARM-BRACELET-0) 0)
(check-expect (bracelet-cost CHARM-BRACELET-1) 15)
(check-expect (bracelet-cost CHARM-BRACELET-2) 27)
(check-expect (bracelet-cost CHARM-BRACELET-3) 37)

(define (bracelet-cost c)
  (cond
    [(boolean? c) 0]
    [else (+ (material-cost (charmBracelet-charm c))
             (bracelet-cost (charmBracelet-charmBracelet c)))]))


;;                               Exercise 2
;; ======================================================================
(define-struct bead [color size])
; A Bead is a (make-bead String Number)
; Interpretation: A bead with a color and size
;  - Color is the color of the bead
;  - Size is the size of the bead

; make-bead : String Number -> Bead
; bead? : Any -> Boolean
; bead-color : Bead -> String
; bead-size : Bead -> Number

(define BEAD-1 (make-bead "red" 5))
(define BEAD-2 (make-bead "blue" 3))
(define BEAD-3 (make-bead "green" 7))


; A FancyBracelet is one of
; - Charm 
; - Bead
; Interpretation: A fancy bracelet with a charm or a bead
; - Charm is the charm on the bracelet
; - Bead is the bead on the bracelet

(define-struct fancyBracelet [item fancyBracelet])

; make-fancyBracelet : Charm/Bead FancyBracelet -> FancyBracelet
; fancyBracelet? : Any -> Boolean
; fancyBracelet-item : FancyBracelet -> Charm/Bead
; fancyBracelet-fancyBracelet : FancyBracelet -> FancyBracelet

(define FANCY-BRACELET-0 #false)
(define FANCY-BRACELET-1 (make-fancyBracelet CHARM-1 FANCY-BRACELET-0))
(define FANCY-BRACELET-2 (make-fancyBracelet BEAD-1 FANCY-BRACELET-1))
(define FANCY-BRACELET-3 (make-fancyBracelet CHARM-2 FANCY-BRACELET-2))
(define FANCY-BRACELET-4 (make-fancyBracelet BEAD-2 FANCY-BRACELET-3))
(define FANCY-BRACELET-5 (make-fancyBracelet CHARM-3 FANCY-BRACELET-4))
(define FANCY-BRACELET-6 (make-fancyBracelet BEAD-3 FANCY-BRACELET-5))

(define (fancyBracelet-temp t)
  (cond
    [(fancyBracelet? t)
     (... (charm-temp (fancyBracelet-item t)) ...
          (fancyBracelet-temp (fancyBracelet-fancyBracelet t)) ...)]
    [else
     (...)]))

; count-charms: FancyBracelet -> Number
; returns the number of charms on the bracelet
(check-expect (count-charms FANCY-BRACELET-0) 0)
(check-expect (count-charms FANCY-BRACELET-1) 1)
(check-expect (count-charms FANCY-BRACELET-2) 1)
(check-expect (count-charms FANCY-BRACELET-3) 2)
(check-expect (count-charms FANCY-BRACELET-4) 2)
(check-expect (count-charms FANCY-BRACELET-5) 3)
(check-expect (count-charms FANCY-BRACELET-6) 3)


(define (count-charms f)
  (cond
    [(boolean? f) 0]
    [(bead? (fancyBracelet-item f)) (count-charms (fancyBracelet-fancyBracelet f))]
    [else (+ 1 (count-charms (fancyBracelet-fancyBracelet f)))]))


; swap-bead: Bead String -> Bead/Charm
; given a Bead returns a charm if the bead matches
; color. Otherwise returns the Bead
(check-expect (swap-bead BEAD-1 "red" "moon") (make-charm "silver" "moon"))
(check-expect (swap-bead BEAD-1 "blue" "jeff") BEAD-1)
(check-expect (swap-bead BEAD-2 "blue" "heart") (make-charm "silver" "heart"))
(check-expect (swap-bead BEAD-2 "red" "bob") BEAD-2)

(define (swap-bead b c cf)
  (cond
    [(string=? (bead-color b) c) (make-charm "silver" cf)]
    [else b]))


;; ======================================================================
;; Fix!
; upgrade-bracelet: FancyBracelet Color -> FancyBracelet
; returns an upgraded exchanges all of the beads of the
; given color in the bracelet for silver charms
(check-expect (upgrade-bracelet FANCY-BRACELET-0 "red" "moon") FANCY-BRACELET-0)
(check-expect (upgrade-bracelet FANCY-BRACELET-1 "red" "star") FANCY-BRACELET-1)



(define (upgrade-bracelet f c cf)
  (cond
    [(boolean? f) f]
    [else (make-fancyBracelet
           (cond
             [(bead? (fancyBracelet-item f)) (swap-bead (fancyBracelet-item f) c cf)]
             [else (fancyBracelet-item f)])
           (upgrade-bracelet (fancyBracelet-fancyBracelet f) c cf))]))