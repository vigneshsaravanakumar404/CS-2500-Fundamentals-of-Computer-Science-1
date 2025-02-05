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