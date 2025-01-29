;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/string)

; =========================================================================
;; a.
; A Name is a String
; Interpretation: A name of a hotel
(define NAME-1 "Marriott")
(define NAME-2 "Hilton")
(define NAME-3 "Sheraton")

(define (name-temp n)
  (... n ...))


; A Stars is a natural number 1 to 5
; Interpretation: Luxry rating of the hotel in stars from 1 to 5

(define STARS-1 3)
(define STARS-2 4)
(define STARS-3 5)

(define (stars-temp s)
  (... s ...))


; A Price is a String
; Interpretation: Price of the hotel in $ signs 1 to 4
(define PRICE-1 "$")
(define PRICE-2 "$$")
(define PRICE-3 "$$$")
(define PRICE-4 "$$$$")

(define (price-temp p)
  (... p ...))


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
(check-expect (in-budget? HOTEL-1 PRICE-2) #t)
(check-expect (in-budget? HOTEL-3 PRICE-2) #f)
(check-expect (in-budget? HOTEL-3 PRICE-4) #t)

(define (in-budget? h p)
  (cond
    [(< (string-length p) (string-length (hotel-price h))) #f]
    [else #t]))
;; =========================================================================



;; =========================================================================
;; c.
;; make-cheaper : Hotel -> Hotel
;; Given a hotel makes the price 1 cheaper till "$" and adds cheaper to name
(check-expect (make-cheaper HOTEL-1)
              (make-hotel "Cheaper Marriott" 3 "$"))
(check-expect (make-cheaper HOTEL-2)
              (make-hotel "Cheaper Hilton" 4 "$"))
(check-expect (make-cheaper HOTEL-3)
              (make-hotel "Cheaper Sheraton" 5 "$$"))
(check-expect (make-cheaper HOTEL-4)
              (make-hotel "Cheaper Marriott" 4 "$$$"))

(define (make-cheaper h)
  (cond
    [(string=? (hotel-price h) "$")
     (make-hotel (string-append "Cheaper " (hotel-name h))
                 (hotel-stars h)
                 "$")]
    [else (make-hotel (string-append "Cheaper " (hotel-name h))
                      (hotel-stars h)
                      (substring (hotel-price h) 1))]))
;; =========================================================================



;; =========================================================================
;; d.
;; star-count->stars : Number -> String;
;; Given star count returns string with that many stars
(define (star-count->stars n)
  (cond
    [(= n 5) "★★★★★"]
    [(= n 4) "★★★★"]
    [(= n 3) "★★★"]
    [(= n 2) "★★"]
    [else "★"]))


;; draw-panels : Hotel -> Image
;; Given a Hotel returns a card with the hotel's information
(define (draw-panels h)
  (place-images (list (text (hotel-name h) 40 "black")
                      (text (hotel-price h) 20 "forest green")
                      (text (star-count->stars (hotel-stars h)) 20 "red")
                      (rectangle 498 98 "solid" "white"))
                (list (make-posn 250 20)
                      (make-posn 250 50)
                      (make-posn 250 75)
                      (make-posn 250 50))
                (rectangle 500 100 "solid" "black")))

;; =========================================================================

