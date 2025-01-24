;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


;; ====================================================
;; Exercise 1
;; A position is a struct that stores a make-posn
;; ====================================================

;; ====================================================
;; Exercise 2

; A Position is a (make-posn Number Number)
; Int.: the x and y coordinates of

;; pos-adder Position -> Position
;; Given two Positions returns the addition of the two coordinates
(check-expect (make-posn 10 10)
              (pos-adder (make-posn 5 5) (make-posn 5 5)))
(check-expect (make-posn 5 10)
              (pos-adder (make-posn 2 5) (make-posn 3 5)))

(define (pos-adder p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))
;; ====================================================


;; ====================================================
;; Exercise 3
;; A Book can't be a make-posn because that is already defined. 
;; Instead it should be something like make-book
;; ====================================================


;; ====================================================
;; Exercise 4
(define-struct animal [name species inside/outside diet])

;; An Animal is a (make-animal String String Boolean String)
;; - name is the name of the animal
;; - species is the species the animal is
;; - inside/outside is a boolean determining if the animal is outdoors or indoors
;; - Diet is one of herbivorous, carnivorous, or omnivorous
;; Interpretation: an animal with a name, species, location, and diet
;; Example: (make-animal "Fido" "Dog" #t "Carnivorous")
(define (animal-temp nml)
  (...
    (nml)...
    (posn-y p)...))
;; ====================================================


;; ====================================================
;; Exercise 5

(define-struct address [num st city us-state zip])
; An Address is a (make-address Nat String String String Nat) where
; - num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in, and
; - zip is the zipcode of the building
;; Interpretation: an address with a building #, street, city, state and zipcode
;; Example: (make-address 1 "Wall Street" "New York" "New York" 10005)
(define (address-temp nml)
  (...
    (nml)...
    (posn-y p)...))


(define-struct student [first last nuid local perm])
; An NEUStudent is a
; (make-student String String PositiveNumber Address Address) where
; - first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID #
; - local is the student's local address, and
; - perm is the student's permanent address
;; Interpretation: represents a NEU student
;; Example: (make-NEUStudent "Vignesh" 
;;                           "Saravanakumar" 
;;                           000123322 
;;                           (make-address 1 "Wall Street" "New York" "New York" 10005) 
;;                           (make-address 1 "Wall Street" "New York" "New York" 10005))

(define (student-template nml)
  (...
    (nml)...
    (posn-y p)...))
;; ====================================================


;; ====================================================
;; Exercise 6
(define s1 (make-student "Vignesh" 
			 "Saravanakumar" 
			 000123322
                         (make-address 1 "Wall Street" "New York" "New York" 10005) 
			 (make-address 1 "Wall Street" "New York" "New York" 10005)))

;; student-email : Student -> String
;; Given a student returns the student's email address
(check-expect (student-email s1) "saravanakumar.v@northeastern.edu")

(define (student-email student)
  (string-append (string-downcase (student-last student)) 
                 "." 
                 (string-downcase (substring (student-first student) 0 1)) 
                 "@northeastern.edu"))
;; ====================================================


;; ====================================================
;; Exercise 7
(check-expect (make-student "Vignesh"
                            "Saravanakumar"
                            000123322
                            (make-address 1 "Wall Street" "New York" "New York" 10005)
                            (make-address 1 "Wall Street" "New York" "New York" 1))
              (new-zipcode (make-student "Vignesh"
                                         "Saravanakumar"
                                         000123322
                                         (make-address 1 "Wall Street" "New York" "New York" 10005)
                                         (make-address 1 "Wall Street" "New York" "New York" 1))
                           1))
;; new-zipcode: Student Number -> Student
;; Given a student replaces thier permanent zip-code with a new one
(define (new-zipcode student zip)
  (make-student (student-first student)
                (student-last student)
                (student-nuid student)
                (student-local student)
                (make-address (address-num (student-perm student))
                              (address-st (student-perm student))
                              (address-city (student-perm student))
                              (address-us-state (student-perm student))
                              zip)))
;; ====================================================


;; ====================================================
;; Exercise 8
(define BG (rectangle 500 500 "solid" "black"))
(define DOT (circle 5 "solid" "white"))
;; ====================================================


;; ====================================================
;; Exercise 9

;; handle-mouse: Position Number Number String -> Position
;; Given a state, x, y, and result returns a new position
(define (handle-mouse state x y result)
  (cond [(string=? result "button-down") (make-posn x y)]
        [else state]))

;; render: Position -> Image
(define (render state)
  (place-image DOT (posn-x state) (posn-y state) BG))
;; ====================================================


;; ====================================================
;; Exercise 10
(big-bang (make-posn 50 50)
  (on-mouse handle-mouse)
  (to-draw render))     
;; ====================================================