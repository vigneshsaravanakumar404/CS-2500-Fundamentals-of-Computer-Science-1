(require 2htdp/universe)
(require 2htdp/image)

; =========================================================================
;; a.
(define-struct name [name])

; A Name is a (make-name String)
; Interpretation: A name of a hotel
;  - name is the name of the hotel

; make-name : String -> Name
; name? : Any -> Boolean
; name-name : Name -> String

(define NAME-1 (make-name "Marriott"))
(define NAME-2 (make-name "Hilton"))
(define NAME-3 (make-name "Sheraton"))

(define (name-temp n)
  (... (name-name n) ...))



(define-struct stars [stars])

; A Stars is a (make-stars Number)
; Interpretation: Luxry rating of the hotel in stars from 1 to 5

; make-stars : Number -> Stars
; stars? : Any -> Boolean
; stars-stars : Stars -> Number

(define STARS-1 (make-stars 3))
(define STARS-2 (make-stars 4))
(define STARS-3 (make-stars 5)) 

(define (stars-temp s)
  (... (stars-stars s) ...))



(define-struct price [price])

; A Price is a (make-price String)
; Interpretation: Price of the hotelin $ signs 1 to 4

; make-price : String -> Price
; price? : Any -> Boolean
; price-price : Price -> String

(define PRICE-1 (make-price "$"))
(define PRICE-2 (make-price "$$"))
(define PRICE-3 (make-price "$$$"))
(define PRICE-4 (make-price "$$$$"))


(define (price-temp p)
  (... (price-price p) ...))



(define-struct hotel [name stars price])

; A Hotel is a (make-hotel String Number String)
; Interpretation: A hotel with a name, number of stars, and price
;  - name is the name of the hotel

; make-hotel : String Number Number -> Hotel
; hotel? : Any -> Boolean
; hotel-name : Hotel -> String
; hotel-stars : Hotel -> Number
; hotel-price : Hotel -> String

(define HOTEL-1 (make-hotel NAME-1 STARS-1 PRICE-1))
(define HOTEL-2 (make-hotel NAME-2 STARS-2 PRICE-2))
(define HOTEL-3 (make-hotel NAME-3 STARS-3 PRICE-3))
(define HOTEL-4 (make-hotel NAME-1 STARS-2 PRICE-4))

(define (hotel-temp h)
  (... (hotel-stars h)
       (hotel-name h)
       (hotel-price h) ...))
;; =========================================================================



;; =========================================================================
;; b.
;; in-budget? : Hotel Price -> Boolean
;; Determines if the price of the hotel is in the budget
(check-expect (in-budget? HOTEL-1 PRICE-1) #t)
(check-expect (in-budget? HOTEL-2 PRICE-2) #t)
(check-expect (in-budget? HOTEL-4 PRICE-4) #f)
(check-expect (in-budget? HOTEL-1 PRICE-2) #f)

(define (in-budget? h p)
 (cond
   [(> (string-length (price-price (hotel-price h))) (string-length (price-price p))) #f]
   [else #t]))
;; =========================================================================



;; =========================================================================
;; c.

;; =========================================================================



;; =========================================================================
;; d.

;; =========================================================================