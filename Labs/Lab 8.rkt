;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lab 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; Exercise 1
; items_rejected? : (X) [List-of X] Predicate -> Boolean
; Returns whether the predicate rejects any items in the list
(check-expect (items_rejected? (list -5 -4 -3 -2 -1 0 1 2 3 4) positive?) #true)
(check-expect (items_rejected? (list -5 -4 -3 -2 -1 0 1 2 3 4) negative?) #true)
(check-expect (items_rejected? (list) zero?) #false)
(define (items_rejected? loa pred)
  (< (length (filter pred loa)) (length loa)))

; Exercise 2
; build-list : Number -> [List-of Number]
; Returns a list of numbers from 0 to n where the odd numbers are negative
(check-expect (create-list 5) '(0 -1 2 -3 4))
(check-expect (create-list 6) '(0 -1 2 -3 4 -5))
(check-expect (create-list 7) '(0 -1 2 -3 4 -5 6))
(check-expect (create-list 0) '())


(define (create-list n)
  (local [(define (helper x)
            (if (odd? x) (- x) x))]
    (map helper (range 0 n 1))))


; Exercise 3
; return-shorter : [List-of String] [List-of String] -> [List-of String]
; Returns the element that has the shorter length in each index
(check-expect (return-shorter '("a" "bb" "ccc") '("a" "bb" "ccc")) '("a" "bb" "ccc"))
(check-expect (return-shorter '("aaaaaaaa" "bbbbbbbbbbb" "ccccccccccc") '("a" "bb" "cc")) '("a" "bb" "cc"))
(check-expect (return-shorter (list "aaa" "b" "ccc") (list "a" "bbb" "c")) (list "a" "b" "c"))

(define (return-shorter l1 l2)
  (local [(define (helper x y)
            (if (< (string-length x) (string-length y)) x y))]
    (map helper l1 l2)))


; Exercise 4
(define-struct selfdrivingcar [make model color position])

; A SelfDrivingCar is a (make-selfdrivingcar String String String func)
; Interpretation: Represents a car
; - make is a String representing make of car
; - model is a String representing model of car
; - color is a String representing color of car

(define SELFDRIVINGCAR-1 (make-selfdrivingcar "Tesla" "3" "red" (lambda (x) (sqrt x))))
(define SELFDRIVINGCAR-2
  (make-selfdrivingcar "Ford" "Fusion" "green" (lambda (x) (+ x 5 (expt x 3)))))
(define SELFDRIVINGCAR-3
  (make-selfdrivingcar "Banshee" "GTS" "blue" (lambda (x) (+ x 15 (expt x 2)))))

(define (my-struct-temp f)
  (... (my-struct-f1 f) ...
       (my-struct-f2 f) ...
       (my-struct-f3 f) ...))

; show-all-cars : [List-of SelfDrivingCar] Number -> Image
; takes a list of SelfDrivingCars and a time (a Number) and draw all the cars in the list at the correct position for the given time
(check-expect (show-all-cars '() 5) (rectangle 1000 400 "solid" "gray"))
(check-expect 
  (show-all-cars (list SELFDRIVINGCAR-1) 4)
  (place-image
    (circle 10 "solid" "red")
    ((selfdrivingcar-position SELFDRIVINGCAR-1) 4)
    200
    (rectangle 1000 400 "solid" "gray")))
(check-expect 
  (show-all-cars (list SELFDRIVINGCAR-1 SELFDRIVINGCAR-2) 3)
  (place-image
    (circle 10 "solid" "red")
    ((selfdrivingcar-position SELFDRIVINGCAR-1) 3)
    200
    (place-image
      (circle 10 "solid" "green")
      ((selfdrivingcar-position SELFDRIVINGCAR-2) 3)
      200
      (rectangle 1000 400 "solid" "gray"))))
(check-expect 
  (show-all-cars (list SELFDRIVINGCAR-1 SELFDRIVINGCAR-2 SELFDRIVINGCAR-3) 2)
  (place-image
    (circle 10 "solid" "red")
    ((selfdrivingcar-position SELFDRIVINGCAR-1) 2)
    200
    (place-image
      (circle 10 "solid" "green")
      ((selfdrivingcar-position SELFDRIVINGCAR-2) 2)
      200
      (place-image
        (circle 10 "solid" "blue")
        ((selfdrivingcar-position SELFDRIVINGCAR-3) 2)
        200
        (rectangle 1000 400 "solid" "gray")))))

(define (show-all-cars locs t)
    (cond
      [(empty? locs) (rectangle 1000 400 "solid" "gray")]
      [else
         (place-image
          (circle 10 "solid" (selfdrivingcar-color (first locs)))
          ((selfdrivingcar-position (first locs)) t)
          200
          (show-all-cars (rest locs) t))]))

(show-all-cars (list SELFDRIVINGCAR-1 SELFDRIVINGCAR-2 SELFDRIVINGCAR-3) 8)

; Exercise 6
; my-function : (X Y Z) [List-of X] [Z -> [X -> Y]] Z -> [X [List-of U] -> [List-of Y]]
; Returns a function that takes a list of X and returns a function that takes a list of U and returns a list of Y

(define (my-function x y z)
  (lambda (x) (map (y z) x)))
