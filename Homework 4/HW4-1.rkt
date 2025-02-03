;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; month-abbrev : Month -> String
; Given a Month, returns the first three letters of the month
(check-expect (month-abbrev "January") "Jan")
(check-expect (month-abbrev "February") "Feb")
(check-expect (month-abbrev "March") "Mar")

(define (month-abbrev m)
  (substring m 0 3))
;; =========================================================================


;; =========================================================================
(define-struct date [month day year])

; A Date is a (make-date Month Number Number)
; Interpretation: A date in the format month/day/year
;  - Month is the month
;  - Day is the day
;  - Year is the year

; make-date : Month Number Number -> Date
; date? : Any -> Boolean
; date-month : Date -> Month
; date-day : Date -> Number
; date-year : Date -> Number

(define DATE-1 (make-date "January" 1 2018))
(define DATE-2 (make-date "November" 15 2005))
(define DATE-3 (make-date "March" 17 2006))

(define (date-temp d)
  (... (month-abbrev (date-month d)) ...
       (number->string (date-day d)) ...
       (number->string (date-year d)) ...))

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