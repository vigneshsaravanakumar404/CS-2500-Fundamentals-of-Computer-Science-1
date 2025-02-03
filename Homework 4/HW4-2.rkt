;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =========================================================================
(define-struct name [first middle last])
(define-struct date [month day year])

(define (name->string n)
  (string-append (name-first n)
                 " "
                 (name-middle n)
                 ". "
                 (name-last n)))

(define (month-abbrev m)
  (substring m 0 3))

(define (date->string d)
  (string-append (month-abbrev (date-month d))
                 ". "
                 (number->string (date-day d))
                 ", "
                 (number->string (date-year d))))
;; =========================================================================

(define-struct student [name date tution])

; A Student is a (make-student Name Date Number)
; Interpretation: A Representation of a NEU Student
;  - Name is the student's name
;  - Date is the student's date of birth
;  - Number is the student's tution

; make-student : Name Date Number -> Student
; student? : Any -> Boolean
; student-name : Student -> Name
; stident-date : Student -> Date
; student-tution : Student -> Number

(define STUDENT-1
  (make-student (make-name "John" "Doe" "Smith")
                (make-date "January" 1 1990)
                40000))
(define STUDENT-2
  (make-student (make-name "Jane" "R" "Doe")
                (make-date "February" 2 1991)
                39500))
(define STUDENT-3
  (make-student (make-name "Jim" "S" "Smith")
                (make-date "March" 3 1992)
                39000))

(define (student-temp t)
  (... (name->string (student-name t)) ...
       (date->string (student-date t)) ...
       (number->string (student-tution t)) ...))

;; end-of-enrollment : Student -> Number
;; Given a Student, returns the year the student will graduate
(check-expect (end-of-enrollment STUDENT-1) 1994)
(check-expect (end-of-enrollment STUDENT-2) 1995)
(check-expect (end-of-enrollment STUDENT-3) 1996)

(define (end-of-enrollment s)
  (+ 4 (date-year (student-date s))))


;; annual-tuition : Student -> Number
;; Given a Student, returns the annual tuition
(check-expect (annual-tuition STUDENT-1) 80000)
(check-expect (annual-tuition STUDENT-2) 79000)
(check-expect (annual-tuition STUDENT-3) 78000)

(define (annual-tuition s)
  (* 2 (student-tution s)))
;; =========================================================================


;; =========================================================================
(define-struct faculty [name date contract salary])

; A Faculty is a (make-faculty Name Date Number Number)
; Interpretation: A Representation of a NEU Faculty
;  - Name is the faculty's name
;  - Date is the start of this contract
;  - Contract is the number of years of the contract
;  - Salary is the faculty's salary

; make-faculty : Name Date Number Number -> Faculty
; faculty? : Any -> Boolean
; faculty-name : Faculty -> Name
; faculty-date : Faculty -> Date
; faculty-contract : Faculty -> Number
; faculty-salary : Faculty -> Number

(define FACULTY-1
  (make-faculty (make-name "John" "Doe" "Smith")
                (make-date "January" 1 1990)
                4
                80000))
(define FACULTY-2
  (make-faculty (make-name "Jane" "R" "Doe")
                (make-date "February" 2 1991)
                4
                90000))
(define FACULTY-3
  (make-faculty (make-name "Jim" "S" "Smith")
                (make-date "March" 3 1992)
                4
                100000))

(define (faculty-temp f)
  (... (name->string (faculty-name f)) ...
       (date->string (faculty-date f)) ...
       (number->string (faculty-contract f)) ...
       (number->string (faculty-salary f)) ...))

; end-of-contract : Faculty -> Number
; Given a Faculty, returns the year the contract ends
(check-expect (end-of-contract FACULTY-1) 1994)
(check-expect (end-of-contract FACULTY-2) 1995)
(check-expect (end-of-contract FACULTY-3) 1996)

(define (end-of-contract f)
  (+ (date-year (faculty-date f))
     (faculty-contract f)))

; takehome-salary : Faculty -> Number
; Given a Faculty, returns the takehome salary
(check-expect (takehome-salary FACULTY-1) 60000)
(check-expect (takehome-salary FACULTY-2) 67500)
(check-expect (takehome-salary FACULTY-3) 75000)

(define (takehome-salary f)
  (* 0.75 (faculty-salary f)))
;; =========================================================================


;; =========================================================================
(define-struct staff [name date salary])

; A Staff is a (make-staff Name Date Number)
; Interpretation: A Representation of a NEU Staff
;  - Name is the staff's name
;  - Date is the start of this contract
;  - Salary is the staff's hour wage

; make-staff : Name Date Number -> Staff
; staff? : Any -> Boolean
; staff-name : Staff -> Name
; staff-date : Staff -> Date
; staff-salary : Staff -> Number

(define STAFF-1
  (make-staff (make-name "John" "Doe" "Smith")
              (make-date "January" 1 1990)
              25))
(define STAFF-2
  (make-staff (make-name "Jane" "R" "Doe")
              (make-date "February" 2 1991)
              30))
(define STAFF-3
  (make-staff (make-name "Jim" "S" "Smith")
              (make-date "March" 3 1992)
              35))

(define (staff-temp s)
  (... (name->string (staff-name s)) ...
       (date->string (staff-date s)) ...
       (number->string (staff-salary s)) ...))

; annual-compensation: Staff -> Number
; Given a Staff, returns the annual compensation
(check-expect (annual-compensation STAFF-1) 50000)
(check-expect (annual-compensation STAFF-2) 60000)
(check-expect (annual-compensation STAFF-3) 70000)

(define (annual-compensation s)
  (* 2000 (staff-salary s)))
;; =========================================================================
        
