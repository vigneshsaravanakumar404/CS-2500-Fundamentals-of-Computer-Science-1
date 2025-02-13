;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;                              Exercise 1
;; ======================================================================

;; A Material is one of:
; - "gold"
; - "silver"
; - "pewter"
; Interpretation: A meterial represents what the Charm is made of

(define MATERIAL-1 "gold")
(define MATERIAL-2 "silver")
(define MATERIAL-3 "pewter")

(define-struct charm [description material])

(define (material-temp m)
  (... (cond
         [(string=? m "gold") ...]
         [(string=? m "silver") ...]
         [(string=? m "pewter") ...]) ...))

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
;  - #flase represents the clasp of the bracelet

; make-charmBracelet : Charm CharmBracelet -> CharmBracelet
; charmBracelet? : Any -> Boolean
; charmBracelet-charm : CharmBracelet -> Charm
; charmBracelet-charmBracelet : CharmBracelet -> CharmBracelet

(define CHARM-BRACELET-0 #false)
(define CHARM-BRACELET-1 (make-charmBracelet CHARM-1 CHARM-BRACELET-0))
(define CHARM-BRACELET-2 (make-charmBracelet CHARM-2 CHARM-BRACELET-1))
(define CHARM-BRACELET-3 (make-charmBracelet CHARM-3 CHARM-BRACELET-2))


(define (charmBracelet-temp t)
  (...
   (cond
     [(boolean? cf)]
     [(charmBracelet? c)
      (... (charm-temp (charmBracelet-charm c)) ...
           (charmBracelet-temp (charmBracelet-charmBracelet c)) ...)])))
      

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
    [(charmBracelet? c) (+ (material-cost (charmBracelet-charm c))
                           (bracelet-cost (charmBracelet-charmBracelet c)))]))


;; Exercise 2
;; ======================================================================
;; Fix!
; upgrade-bracelet: FancyBracelet Color String -> FancyBracelet
; returns an upgraded exchanges all of the beads of the
; given color in the bracelet for silver charms of given figure
(check-expect (upgrade-bracelet FANCY-BRACELET-0 "red" "moon") FANCY-BRACELET-0)
(check-expect (upgrade-bracelet FANCY-BRACELET-1 "red" "star") FANCY-BRACELET-1)
(check-expect (upgrade-bracelet (make-fancyBracelet (make-bead "red" 5) #false) "red" "hello")
              (make-fancyBracelet (make-charm "hello" "silver") #false))
(check-expect (upgrade-bracelet FANCY-BRACELET-6 "green" "diamond") 
              (make-fancyBracelet (make-charm "diamond" "silver") FANCY-BRACELET-5))
(check-expect (upgrade-bracelet FANCY-BRACELET-6 "blue" "sun")
              (make-fancyBracelet
               BEAD-3
               (make-fancyBraceletCHARM-3
                (make-fancyBracelet
                 (make-charm "sun" "silver")
                 (make-fancyBracelet
                  CHARM-2
                  (make-fancyBracelet
                   BEAD-1
                   (make-fancyBracelet CHARM-1 #false)))))))


(define (upgrade-bracelet f c cf)
  (cond
    [(boolean? f) f]
    [else (make-fancyBracelet
           (cond
             [(bead? (fancyBracelet-item f)) (swap-bead (fancyBracelet-item f) c cf)]
             [else (fancyBracelet-item f)])
           (upgrade-bracelet (fancyBracelet-fancyBracelet f) c cf))]))