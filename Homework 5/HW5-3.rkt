#lang htdp/bsl

(define-struct student [firstname lastname gpa on-coop])

; A Student is a (make-student String String Number Boolean)
; Interpretation: A (make-student fn ln g c) represents a
; - Northeastern student whose first name is fn and last name is ln, with
; - cumulative grade point average g, and for whom c is #true if they are
; - currently doing a coop experience this term and #false otherwise.

(define STUDENT-1 (make-student "Jane" "Smith" 4.0 #true))
(define STUDENT-2 (make-student "Ashok" "Singhal" 0.0 #false))
(define STUDENT-3 (make-student "Alice" "Wonderland" 3.5 #true))
(define STUDENT-4 (make-student "Bob" "Builder" 2.0 #false))

(define (student-templ st)
  (... (student-firstname st) ...
       (student-lastname st) ...
       (student-gpa st) ...
       (student-on-coop st) ...))



; A ListOfStudents (LoS) is one of
; - '()
; - (cons Student LoS)
; Interpretation: Represents a list of Students

(define LOS-0 '())
(define LOS-1 (cons STUDENT-1 LOS-0))
(define LOS-2 (cons STUDENT-2 LOS-1))
(define LOS-3 (cons STUDENT-3 LOS-2))
(define LOS-4 (cons STUDENT-4 LOS-3))

(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) (... (student-templ (first los)) ...
                          (los-temp (rest los)) ...)]))


; count-coop-students : Los -> Number
; Counts the number of students on coop
(check-expect (count-coop-students LOS-0) 0)
(check-expect (count-coop-students LOS-1) 1)
(check-expect (count-coop-students LOS-2) 1)
(check-expect (count-coop-students LOS-3) 2)
(check-expect (count-coop-students LOS-4) 2)

(define (count-coop-students los)
  (cond
    [(empty? los) 0]
    [(cons? los) (if (student-on-coop (first los))
                     (+ 1 (count-coop-students (rest los)))
                     (count-coop-students (rest los)))]))

; exchange-coop-students : Los -> Los
; Flips the coop status of all students
(check-expect (exchange-coop-students LOS-0) LOS-0)
(check-expect (exchange-coop-students LOS-1)
              (cons (make-student "Jane" "Smith" 4.0 #false) LOS-0))
(check-expect (exchange-coop-students LOS-2)
              (cons (make-student "Ashok" "Singhal" 0.0 #true)
                    (cons (make-student "Jane" "Smith" 4.0 #false) LOS-0)))
(check-expect (exchange-coop-students LOS-3)
              (cons (make-student "Alice" "Wonderland" 3.5 #false)
                    (cons (make-student "Ashok" "Singhal" 0.0 #true)
                          (cons (make-student "Jane" "Smith" 4.0 #false) LOS-0))))
(check-expect (exchange-coop-students LOS-4)
              (cons (make-student "Bob" "Builder" 2.0 #true)
                    (cons (make-student "Alice" "Wonderland" 3.5 #false)
                          (cons (make-student "Ashok" "Singhal" 0.0 #true)
                                (cons (make-student "Jane" "Smith" 4.0 #false) LOS-0)))))

(define (exchange-coop-students los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (make-student (student-firstname (first los))
                                     (student-lastname (first los))
                                     (student-gpa (first los))
                                     (not (student-on-coop (first los))))
                       (exchange-coop-students (rest los)))]))
