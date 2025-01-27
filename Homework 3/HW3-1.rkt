#lang htdp/bsl


;; =========================================================================
(define-struct book [title author year])

; A Book is a (make-book String String Number)
; Interpretation: A book with a title, author and publication year
;  - title is the title of the book
;  - author is the name of the author
;  - year is the year of publication
     
; make-book : String String Number -> Book
; book? : Any -> Boolean
; book-title : book -> String
; book-author : book -> String
; book-year : book -> Number
     
(define BOOK-1 (make-book "Harry Potter" "J.K. Rowling" 1997))
(define BOOK-2 (make-book "The Great Gatsby" "F. Scott Fitzgerald" 1925))
(define BOOK-3 (make-book "The Catcher in the Rye" "J.D. Salinger" 1951))
     
(define (book-temp s)
  (... (book-title s) ...
       (book-author s) ...
       (book-year s) ...))
;; =========================================================================


;; =========================================================================
(define-struct date [day month year])

; A Date is a (make-date Number Number Number)
; Interpretation: A date with a day, month and year
;  - day is the day of the month
;  - month is the month of the year
;  - year is the year

; make-date : Number Number Number -> Date
; date? : Any -> Boolean
; date-day : date -> Number
; date-month : date -> Number
; date-year : date -> Number

(define DATE-1 (make-date 17 3 2006))
(define DATE-2 (make-date 6 5 1898))
(define DATE-3 (make-date 1 2 3456)) 

(define (date-temp d)
  (... (date-day d) ...
       (date-month d) ...
       (date-year d) ...))
;; =========================================================================


;; =========================================================================
(define-struct holiday [title date is-observed])

; A Holiday is a (make-holiday String Date Boolean)
; Interpretation: A holiday with a title, date and whether it is observed
;  - title is the name of the holiday
;  - date is the date of the holiday
;  - is-observed is whether the holiday is observed

; make-holiday : String Date Boolean -> Holiday
; holiday? : Any -> Boolean
; holiday-title : holiday -> String
; holiday-date : holiday -> Date
; holiday-is-observed : holiday -> Boolean

(define HOLIDAY-1 (make-holiday "Christmas" DATE-1 #t))
(define HOLIDAY-2 (make-holiday "Mother's Day" DATE-2 #f))
(define HOLIDAY-3 (make-holiday "New Year's Day" DATE-3 #t))

(define (holiday-temp h)
  (... (holiday-title h) ...
       (date-temp (holiday-date h)) ...
       (holiday-is-observed h) ...))
;; =========================================================================