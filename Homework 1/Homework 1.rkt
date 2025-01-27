;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ====================================================
;; Exercise 1
#| Do not use dangling parentheses: the closing right-parenthesis should be
on the same line as the last expression of your code.

  ;; ------------------------ GOOD

  (define (f l)

    (cond [(empty? l) 0]

          [else (f (rest l))])) ;; HERE

  ;; ------------------------ BAD

  (define (f l)

    (cond [(empty? 1) 0]

          [else (f (rest l))]

     ) ;; NOT HERE

   )

The dangling parentheses in the second code excerpt are considered extremely bad style. |#
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
  (cond
    [(> (string-length word) 0) (string-append (substring word 1) (substring word 0 1) "ay")]
    [else "ay"]))

;; Tests
;; (pig-latinize "crater") should be "ratercay"
;; (pig-latinize "Vignesh") should be "igneshVay"
;; ====================================================


;; ====================================================
;; Exercise 4
;; middle : Any Any Any -> Any
;; Returns the middle value of three inputs
;; Example: (middle "one" "two" "three") => "two"
(define (middle a b c)
  b)

;; Tests
;; (middle "one" "two" "three") should be "two"
;; (middle 3.14 null #f) should be null
;; ====================================================


;; ====================================================
;; Exercise 5
;; cost-to-climb : Number -> Number
;; Calculates the cost to climb a given number of meters
;; Example: (cost-to-climb 3500) => 645.0
(define (cost-to-climb m)
  (+ (* m 0.15) 120))

;; Tests
;; (cost-to-climb 3500) should be 645.0
;; (cost-to-climb 101) should be 135.15
;; ====================================================


;; ====================================================
;; Exercise 6
;; valid-date? : String Number -> Boolean
;; Checks if a given month and day form a valid date
;; Example: (valid-date? "January" 31) => true
(define (valid-date? month day)
  (cond
    [(not (integer? day)) false]
    [(and (or (string=? (string-downcase month) "january")
              (string=? (string-downcase month) "march")
              (string=? (string-downcase month) "may")
              (string=? (string-downcase month) "july")
              (string=? (string-downcase month) "august")
              (string=? (string-downcase month) "october")
              (string=? (string-downcase month) "december"))
          (and (>= day 1) (<= day 31))) true]
    [(and (or (string=? (string-downcase month) "april")
              (string=? (string-downcase month) "june")
              (string=? (string-downcase month) "september")
              (string=? (string-downcase month) "november"))
          (and (>= day 1) (<= day 30))) true]
    [(and (string=? (string-downcase month) "february")
          (and (>= day 1) (<= day 29))) true]
    [else false]))

;; Tests
;; (valid-date? "January" 31)    should be true
;; (valid-date? "April" 31)      should be false
;; (valid-date? "February" 29)   should be true
;; (valid-date? "February" 300)  should be false
;; (valid-date? "NotAMonth" 10)  should be false
;; (valid-date? "May" 3.14)  should be false
;; ====================================================

;; ====================================================
;; Exercise 7
;; image-classify : Image -> String
;; Classifies an image based on its dimensions
;; Example: (image-classify (rectangle 15 15 "solid" "red")) => "square"
(define (image-classify i)
  (cond
    [(= (image-height i) (image-width i)) "square"]
    [(> (image-height i) (image-width i)) "tall"]
    [(< (image-height i) (image-width i)) "wide"]))

;; Tests
;; (image-classify (rectangle 15 15 "solid" "red")) should be "square"
;; (image-classify (rectangle 15 10 "solid" "red")) should be "wide"
;; (image-classify (rectangle 10 15 "solid" "red")) should be "tall"
;; ====================================================


;; ====================================================
;; Exercise 8
(define S 500) ;; RESIZE IMAGE HERE

;; Constants
(define LIGHT-BLUE (make-color 173 216 230))
(define DARK-BROWN (make-color 139 69 19))
(define LIGHT-BROWN (make-color 183 111 87))
(define DARK-GREEN (make-color 34 139 34))
(define GOLD (make-color 255 215 0))
(define ORANGE (make-color 255 165 0))
(define LIGHT-PURPLE (make-color 160 32 240))

;; Shapes
(define BG 
  (rectangle S S "solid" "Light Blue"))
(define CIRCLE 
  (circle (/ S 10.2564102) "solid" "yellow"))
(define GROUND 
  (rectangle S (/ S 5) "solid" "brown"))
(define STUMP 
  (rectangle (/ S 15.3846154) (/ S 6.25) "solid" "light brown"))
(define OUTER-DOOR 
  (rectangle (/ S 22.2222222) (/ S 8.69565217) "solid" "Purple"))
(define WINDOW-1 
  (rectangle (/ S 28.5714286) (/ S 28.5714286) "solid" "white"))
(define LEAVES 
  (ellipse (/ S 5.71428571) (/ S 3.38983051) "solid" "Forest Green"))
(define HOUSE-BASE 
  (rectangle (/ S 4.25531915) (/ S 4.16666667) "solid" "Gold"))
(define ROOF 
  (rotate 180
          (triangle/sss (/ S 3.92156863) (/ S 3.92156863) (/ S 3.38983051) "solid" "red")))
(define WINDOW-2 
  (rectangle (+ (/ S 28.5714286) (/ S 200)) (+ (/ S 28.5714286) (/ S 200)) "solid" "Orange"))

;; Store Objects/Positions
(define objects
  (list LEAVES GROUND STUMP CIRCLE ROOF OUTER-DOOR
        WINDOW-1 WINDOW-1 WINDOW-2 WINDOW-2 HOUSE-BASE))
(define positions
  (list (make-posn (/ S 4.04040404) (/ S 1.95121951)) ;; LEAVES
        (make-posn (/ S 2) (/ S 1.11111111)) ;; GROUND
        (make-posn (/ S 4.04040404) (/ S 1.38888889)) ;; STUMP
        (make-posn (/ S 6.66666667) (/ S 6.66666667)) ;; CIRCLE
        (make-posn (/ S 1.33779264) (/ S 2.13045685)) ;; ROOF
        (make-posn (/ S 1.3400335) (/ S 1.34680135)) ;; OUTER-DOOR 
        (make-posn (/ S 1.42857143) (/ S 1.58730159)) ;; WINDOW-1
        (make-posn (/ S 1.2539185) (/ S 1.58730159)) ;; WINDOW-1
        (make-posn (/ S 1.42857143) (/ S 1.58730159)) ;; WINDOW-2
        (make-posn (/ S 1.2539185) (/ S 1.58730159)) ;; WINDOW-2
        (make-posn (/ S 1.33779264) (/ S 1.46148148)))) ;; HOUSE-BASE

;; Final Scene
(define final-scene
  (place-images objects positions BG))

final-scene
;; ====================================================