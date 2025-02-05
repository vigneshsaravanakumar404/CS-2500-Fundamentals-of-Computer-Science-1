#lang htdp/bsl

;;                                  EXERCISE 1
;; =========================================================================
(define-struct name [first middle last])

; A Name is a (make-name String String String)
; Interpretation: A person's name
;  - First is the first name
;  - Middle is the middle initial
;  - Last is the last name

; make-name : String String String -> Name
; name? : Any -> Boolean
; name-first : Name -> String
; name-middle : Name -> String
; name-last : Name -> String

(define NAME-1 (make-name "John" "Q" "Public"))
(define NAME-2 (make-name "Jane" "R" "Doe"))
(define NAME-3 (make-name "Jim" "S" "Smith"))

(define (name-temp t)
  (... (name-first t) ...
       (name-middle t) ...
       (name-last t) ...))

;; name->string : Name -> String
;; Given a Name, returns the full name as a string
(check-expect (name->string NAME-1) "John Q. Public")
(check-expect (name->string NAME-2) "Jane R. Doe")
(check-expect (name->string NAME-3) "Jim S. Smith")

(define (name->string n)
  (string-append (name-first n)
                 " "
                 (name-middle n)
                 ". "
                 (name-last n)))
;; =========================================================================


;; =========================================================================
; A Month is one of:
; - "January"
; - "February"
; - "March"
; - "April"
; - "May"
; - "June"
; - "July"
; - "August"
; - "September"
; - "October"
; - "November"
; - "December"

(define MONTH-1 "January")
(define MONTH-2 "February")
(define MONTH-3 "March")
(define MONTH-4 "April")
(define MONTH-5 "May")
(define MONTH-6 "June")
(define MONTH-7 "July")
(define MONTH-8 "August")
(define MONTH-9 "September")
(define MONTH-10 "October")
(define MONTH-11 "November")
(define MONTH-12 "December")

; month-temp : Month -> ?
(define (month-temp m)
  (...
   (cond
     [(string=? m "January") ...]
     [(string=? m "February") ...]
     [(string=? m "March") ..]
     [(string=? m "April") ...]
     [(string=? m "May") ...]
     [(string=? m "June") ...]
     [(string=? m "July") ...]
     [(string=? m "August") ...]
     [(string=? m "September") ...]
     [(string=? m "October") ...]
     [(string=? m "November") ...]
     [(string=? m "December") ...])))

; month-abbrev : Month -> String
; Given a Month, returns the first three letters of the month
(check-expect (month-abbrev "January") "Jan")
(check-expect (month-abbrev "February") "Feb")
(check-expect (month-abbrev "March") "Mar")
(check-expect (month-abbrev "May") "May")

(define (month-abbrev m)
  (substring m 0 3))
;; =========================================================================

;; =========================================================================
(define-struct date [month day year])

; A Date is a (make-date Month PosInt PosInt)
; Interpretation: A date in the format month/day/year
;  - Month is the month
;  - Day is the day
;  - Year is the year

; make-date : Month PosInt PosInt -> Date
; date? : Any -> Boolean
; date-month : Date -> Month
; date-day : Date -> PosInt
; date-year : Date -> PosInt

(define DATE-1 (make-date MONTH-1 1 2018))
(define DATE-2 (make-date MONTH-11 15 2005))
(define DATE-3 (make-date MONTH-3 17 2006))

(define (date-temp d)
  (... (date? d) ...
       (date-month d) ...
       (date-day d) ...
       (date-year d) ...))

; date->string : Date -> String
; Given a Date, returns the date as a string
(check-expect (date->string DATE-1) "Jan. 1, 2018")
(check-expect (date->string DATE-2) "Nov. 15, 2005")
(check-expect (date->string DATE-3) "Mar. 17, 2006")

(define (date->string d)
  (string-append (month-abbrev (date-month d))
                 ". "
                 (number->string (date-day d))
                 ", "
                 (number->string (date-year d))))
;; =========================================================================


;;                                  EXERCISE 2
;; =========================================================================
(define-struct student [name date tuition])

; A Student is a (make-student Name Date PosInt)
; Interpretation: A Representation of a NEU Student
;  - Name is the student's name
;  - Date is the student's date of enrollment
;  - PosInt is the student's tuition

; make-student : Name Date PosInt -> Student
; student? : Any -> Boolean
; student-name : Student -> Name
; student-date : Student -> Date
; student-tuition : Student -> PosInt

(define STUDENT-1
  (make-student NAME-1 DATE-1 40000))
(define STUDENT-2
  (make-student NAME-2 DATE-2 39500))
(define STUDENT-3
  (make-student NAME-3 DATE-3 39000))

(define (student-temp t)
  (... (student-name t) ...
       (student-date t) ...
       (student-tuition t) ...))

;; end-of-enrollment : Student -> Date 
;; Given a Student, returns the date the student will graduate
(check-expect (end-of-enrollment STUDENT-1)
              (make-date (date-month DATE-1) (date-day DATE-1) (+ 4 (date-year DATE-1))))
(check-expect (end-of-enrollment STUDENT-2)
              (make-date (date-month DATE-2) (date-day DATE-2) (+ 4 (date-year DATE-2))))
(check-expect (end-of-enrollment STUDENT-3)
              (make-date (date-month DATE-3) (date-day DATE-3) (+ 4 (date-year DATE-3))))

(define (end-of-enrollment s)
  (make-date (date-month (student-date s))
             (date-day (student-date s))
             (+ 4 (date-year (student-date s)))))


;; annual-tuition : Student -> PosInt
;; Given a Student, returns the annual tuition
(check-expect (annual-tuition STUDENT-1) 80000)
(check-expect (annual-tuition STUDENT-2) 79000)
(check-expect (annual-tuition STUDENT-3) 78000)

(define (annual-tuition s)
  (* 2 (student-tuition s)))
;; =========================================================================


;; =========================================================================
(define-struct faculty [name date contract salary])

; A Faculty is a (make-faculty Name Date PosInt PosInt)
; Interpretation: A Representation of a NEU Faculty
;  - Name is the faculty's name
;  - Date is the start of this contract
;  - Contract is the number of years of the contract
;  - Salary is the faculty's salary

; make-faculty : Name Date PosInt PosInt -> Faculty
; faculty? : Any -> Boolean
; faculty-name : Faculty -> Name
; faculty-date : Faculty -> Date
; faculty-contract : Faculty -> PosInt
; faculty-salary : Faculty -> PosInt

(define FACULTY-1
  (make-faculty NAME-1 DATE-1 4 80000))
(define FACULTY-2
  (make-faculty NAME-2 DATE-2 3 90000))
(define FACULTY-3
  (make-faculty NAME-3 DATE-3 5 100000))

(define (faculty-temp f)
  (... (faculty-name f) ...
       (faculty-date f) ...
       (faculty-contract f) ...
       (faculty-salary f) ...))

; end-of-contract : Faculty -> Date
; Given a Faculty, returns the date the contract ends
(check-expect (end-of-contract FACULTY-1)
              (make-date (date-month DATE-1) (date-day DATE-1) (+ 4 (date-year DATE-1))))
(check-expect (end-of-contract FACULTY-2)
              (make-date (date-month DATE-2) (date-day DATE-2) (+ 3 (date-year DATE-2))))
(check-expect (end-of-contract FACULTY-3)
              (make-date (date-month DATE-3) (date-day DATE-3) (+ 5 (date-year DATE-3))))

(define (end-of-contract f)
  (make-date (date-month (faculty-date f))
             (date-day (faculty-date f))
             (+ (faculty-contract f) (date-year (faculty-date f)))))

; takehome-salary : Faculty -> RealNumber
; Given a Faculty, returns the takehome salary
(check-expect (takehome-salary FACULTY-1) 60000) 
(check-expect (takehome-salary FACULTY-2) 67500)
(check-expect (takehome-salary FACULTY-3) 75000)

(define (takehome-salary f)
  (* 0.75 (faculty-salary f)))
;; =========================================================================


;; =========================================================================
(define-struct staff [name date salary])

; A Staff is a (make-staff Name Date PosInt)
; Interpretation: A Representation of a NEU Staff
;  - Name is the staff's name
;  - Date is the start of this contract
;  - Salary is the staff's hour wage

; make-staff : Name Date PosInt -> Staff
; staff? : Any -> Boolean
; staff-name : Staff -> Name
; staff-date : Staff -> Date
; staff-salary : Staff -> PosInt

(define STAFF-1
  (make-staff NAME-1 DATE-1 25))
(define STAFF-2
  (make-staff NAME-2 DATE-2 30))
(define STAFF-3
  (make-staff NAME-3 DATE-3 35))

(define (staff-temp s)
  (... (staff-name s) ...
       (staff-date s) ...
       (staff-salary s) ...))

; annual-compensation: Staff -> PosInt
; Given a Staff, returns the annual compensation
(check-expect (annual-compensation STAFF-1) 50000)
(check-expect (annual-compensation STAFF-2) 60000)
(check-expect (annual-compensation STAFF-3) 70000)

(define (annual-compensation s)
  (* 2000 (staff-salary s)))
;; =========================================================================