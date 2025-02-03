;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =========================================================================
(define-struct name [first middle last])

(define (name->string n)
  (string-append (name-first n)
                 " "
                 (name-middle n)
                 ". "
                 (name-last n)))


(define-struct date [month day year])

(define (month-abbrev m)
  (substring m 0 3))

(define (date->string d)
  (string-append (month-abbrev (date-month d))
                 ". "
                 (number->string (date-day d))
                 ", "
                 (number->string (date-year d))))


(define-struct student [name date tution])

(define (end-of-enrollment s)
  (+ 4 (date-year (student-date s))))

(define (annual-tuition s)
  (* 2 (student-tution s)))


(define-struct faculty [name date contract salary])

(define (end-of-contract f)
  (+ (date-year (faculty-date f))
     (faculty-contract f)))

(define (takehome-salary f)
  (* 0.75 (faculty-salary f)))


(define-struct staff [name date salary])

(define (annual-compensation s)
  (* 2000 (staff-salary s)))
;; =========================================================================



;; =========================================================================
; A NEUPeep is one of:
; - (make-student Name Date Number)
; - (make-faculty Name Date Number Number)
; - (make-staff Name Date Number)
; Interpretation: the people of Northeastern
;  - Student is a Northeastern student
;  - Faculty is a Northeastern faculty member
;  - Staff is a Northeastern staff member /FIX

(define NEUPEEP-1
  (make-student (make-name "John" "Doe" "Smith")
                (make-date "January" 1 1990)
                40000))
(define NEUPEEP-2
  (make-faculty (make-name "Jane" "R" "Doe")
                (make-date "February" 2 1991)
                4
                100000))
(define NEUPEEP-3
  (make-staff (make-name "Jim" "S" "Smith")
              (make-date "March" 3 1992)
              30))

(define (neupeep-temp t)
  (cond
    [(student? t) ...]
    [(faculty? t) ...]
    [(staff? t) ...]))
;; =========================================================================



;; =========================================================================
; end-date : NEUPeep -> Any //FIX
; Given a NEUPeep, returns the end date of their time at Northeastern
(check-expect (end-date NEUPEEP-1) 1994)
(check-expect (end-date NEUPEEP-2) 1995)
(check-expect (end-date NEUPEEP-3) "<UNKNOWN>")

(define (end-date neupeep)
  (cond
    [(student? neupeep) (end-of-enrollment neupeep)]
    [(faculty? neupeep) (end-of-contract neupeep)]
    [(staff? neupeep) "<UNKNOWN>"]))

; annual-budget-cost : NEUPeep -> Number
; Given a NEUPeep, returns the annual cost of their presence at Northeastern
(check-expect (annual-budget-cost NEUPEEP-1) -80000)
(check-expect (annual-budget-cost NEUPEEP-2) 75000)
(check-expect (annual-budget-cost NEUPEEP-3) 60000)

(define (annual-budget-cost neupeep)
  (cond
    [(student? neupeep) (* -1(annual-tuition neupeep))]
    [(faculty? neupeep) (takehome-salary neupeep)]
    [(staff? neupeep) (annual-compensation neupeep)]))
;; ========================================================================