;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Homework1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ====================================================
;; Exercise 1
;; "Do not use dangling parentheses: the closing
;;  right-parenthesis should be on the same line
;;  as the last expression of your code."
;; ====================================================


;; ====================================================
;; Exercise 2
;; numbers, strings, images, and Boolean
;; ====================================================


;; ====================================================
;; Exercise 3
;; pig-latinize : String -> String
;; Converts a word to pig latin form
;; Example: (pig-latinize "crater") => "ratercay"
(define (pig-latinize word)
  (string-append (substring word 1) (substring word 0 1) "ay"))

;; Tests
;; (pig-latinize "crater") should return "ratercay"
;; (pig-latinize "Vignesh") should return "igneshVay"
;; ====================================================


;; ====================================================
;; Exercise 4
;; middle : Any Any Any -> Any
;; Returns the middle value of three inputs
;; Example: (middle "one" "two" "three") => "two"
(define (middle a b c)
  b)

;; Tests
;; (middle "one" "two" "three") should return "two"
;; (middle 3.14 null #f) should return null
;; ====================================================


;; ====================================================
;; Exercise 5
;; cost-to-climb : Number -> Number
;; Calculates the cost to climb a given number of meters
;; Example: (cost-to-climb 3500) => 645.0
(define (cost-to-climb meters)
  (+ 120 (* meters 0.15)))

;; Tests
;; (cost-to-climb 3500) should return 645.0
;; (cost-to-climb 101) should return 135.15
;; ====================================================


;; ====================================================
;; Exercise 6
;; valid-date : String Number -> Boolean
;; Checks if a given month and day form a valid date
;; Example: (valid-date "January" 31) => true
(define (valid-date month day)
  (cond
    [(and (or (string=? month "January")
              (string=? month "March")
              (string=? month "May")
              (string=? month "July")
              (string=? month "August")
              (string=? month "October")
              (string=? month "December"))
          (and (>= day 1) (<= day 31))) true]
    [(and (or (string=? month "April")
              (string=? month "June")
              (string=? month "September")
              (string=? month "November"))
          (and (>= day 1) (<= day 30))) true]
    [(and (string=? month "February")
          (and (>= day 1) (<= day 29))) true]
    [else false]))

;; Tests
;; (valid-date "January" 31)    Expected: true
;; (valid-date "April" 31)      Expected: false
;; (valid-date "February" 29)   Expected: true
;; (valid-date "February" 300)  Expected: false
;; (valid-date "NotAMonth" 10)  Expected: false
;; ====================================================


;; ====================================================
;; Exercise 7
;; image-classify : Image -> String
;; Classifies an image based on its dimensions
;; Example: (image-classify (rectangle 15 15 "solid" "red")) => "square"
(define (image-classify img)
  (cond
    [(= (image-height img) (image-width img)) "square"]
    [(> (image-height img) (image-width img)) "tall"]
    [(< (image-height img) (image-width img)) "wide"]))

;; Tests
;; (image-classify (rectangle 15 15 "solid" "red")) Expected: "square"
;; (image-classify (rectangle 15 10 "solid" "red")) Expected: "wide"
;; (image-classify (rectangle 10 15 "solid" "red")) Expected: "tall"
;; ====================================================


;; ====================================================
;; Exercise 8
(define BG-WIDTH 200) ;; RESIZE IMAGE HERE

;; Constants
(define LIGHT-BLUE (make-color 173 216 230))
(define DARK-BROWN (make-color 139 69 19))
(define LIGHT-BROWN (make-color 183 111 87))
(define DARK-GREEN (make-color 34 139 34))
(define GOLD (make-color 255 215 0))
(define ORANGE (make-color 255 165 0))
(define LIGHT-PURPLE (make-color 160 32 240))

(define CIRCLE-CENTER-X (/ BG-WIDTH 6.66666667))
(define CIRCLE-CENTER-Y (/ BG-WIDTH 6.66666667))
(define CIRCLE-RADIUS (/ BG-WIDTH 13.3333333))
(define ELLIPSE-HEIGHT (/ BG-WIDTH 3.38983051))
(define ELLIPSE-WIDTH (/ BG-WIDTH 5.71428571))
(define ELLIPSE-CENTER-X (/ BG-WIDTH 4.04040404))
(define ELLIPSE-CENTER-Y (/ BG-WIDTH 1.95121951))
(define GROUND-CENTER-X (/ BG-WIDTH 2))
(define GROUND-CENTER-Y (/ BG-WIDTH 1.11111111))
(define GROUND-HEIGHT (/ BG-WIDTH 5))
(define STUMP-WIDTH (/ BG-WIDTH 15.3846154))
(define STUMP-HEIGHT (/ BG-WIDTH 6.25))
(define STUMP-CENTER-Y (/ BG-WIDTH 1.38888889))
(define STUMP-CENTER-X (/ BG-WIDTH 4.04040404))
(define HOUSE-BASE-WIDTH (/ BG-WIDTH 4.25531915))
(define HOUSE-BASE-HEIGHT (/ BG-WIDTH 4.16666667))
(define HOUSE-BASE-X (/ BG-WIDTH 1.33779264))
(define HOUSE-BASE-Y (/ BG-WIDTH 1.46148148))
(define ROOF-L1 (/ BG-WIDTH 3.92156863))
(define ROOF-L2 (/ BG-WIDTH 3.38983051))
(define ROOF-X (/ BG-WIDTH 1.33779264))
(define ROOF-Y (/ BG-WIDTH 2.13045685))
(define OUTER-DOOR-X (/ BG-WIDTH 1.3400335))
(define OUTER-DOOR-Y (/ BG-WIDTH 1.34680135))
(define OUTER-DOOR-WIDTH (/ BG-WIDTH 22.2222222))
(define OUTER-DOOR-HEIGHT (/ BG-WIDTH 8.69565217))
(define WINDOW-INNER-WIDTH (/ BG-WIDTH 28.5714286))
(define WINDOW-OUTER-WIDTH (+ WINDOW-INNER-WIDTH (/ BG-WIDTH 200)))
(define WINDOW-INNER-Y (/ BG-WIDTH 1.58730159))
(define WINDOW-INNER-X-1 (/ BG-WIDTH 1.42857143))
(define WINDOW-INNER-X-2 (/ BG-WIDTH 1.2539185))

;; Shapes
(define BG (rectangle BG-WIDTH BG-WIDTH "solid" LIGHT-BLUE))
(define LEAVES (ellipse ELLIPSE-WIDTH ELLIPSE-HEIGHT "solid" DARK-GREEN))
(define GROUND (rectangle BG-WIDTH GROUND-HEIGHT "solid" DARK-BROWN))
(define STUMP (rectangle STUMP-WIDTH STUMP-HEIGHT "solid" LIGHT-BROWN))
(define HOUSE-BASE (rectangle HOUSE-BASE-WIDTH HOUSE-BASE-HEIGHT "solid" GOLD))
(define ROOF (rotate 180 (triangle/sss ROOF-L1 ROOF-L1 ROOF-L2 "solid" "red")))
(define OUTER-DOOR (rectangle OUTER-DOOR-WIDTH OUTER-DOOR-HEIGHT "solid" LIGHT-PURPLE))
(define WINDOW-1 (rectangle WINDOW-INNER-WIDTH WINDOW-INNER-WIDTH "solid" "white"))
(define WINDOW-2 (rectangle WINDOW-OUTER-WIDTH WINDOW-OUTER-WIDTH "solid" ORANGE))
(define CIRCLE (circle CIRCLE-RADIUS "solid" "yellow"))

;; Final Scene
(define final-scene
  (place-image LEAVES ELLIPSE-CENTER-X ELLIPSE-CENTER-Y
    (place-image GROUND GROUND-CENTER-X GROUND-CENTER-Y
      (place-image STUMP STUMP-CENTER-X STUMP-CENTER-Y
        (place-image CIRCLE CIRCLE-CENTER-X CIRCLE-CENTER-Y
          (place-image ROOF ROOF-X ROOF-Y
            (place-image OUTER-DOOR OUTER-DOOR-X OUTER-DOOR-Y
              (place-image WINDOW-1 WINDOW-INNER-X-1 WINDOW-INNER-Y
                (place-image WINDOW-1 WINDOW-INNER-X-2 WINDOW-INNER-Y
                  (place-image WINDOW-2 WINDOW-INNER-X-1 WINDOW-INNER-Y
                    (place-image WINDOW-2 WINDOW-INNER-X-2 WINDOW-INNER-Y
                      (place-image HOUSE-BASE HOUSE-BASE-X HOUSE-BASE-Y
                        BG))))))))))))

final-scene
;; ====================================================
