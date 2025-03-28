;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


; ======================================== BEGIN IMPORTS ========================================
(define-struct student [name nuid])
; A Student is a (make-student String Number)
; Interpretation: (make-student name nuid) represents a student.
 
(define EX-STUDENT-1 (make-student "Alice" 1))
(define EX-STUDENT-2 (make-student "Bob" 2))
(define EX-STUDENT-3 (make-student "Carol" 3))
 
(define (student-template s)
  (... (student-name s) ... (student-nuid s) ...))
 
(define-struct grade [nuid course value])
; A Grade is a (make-grade Number String Number)
; Interpretation: (make-grade nuid course grade) represents the grade that
; a student received in a course.
 
(define (grade-template g)
  (... (grade-nuid g) ... (grade-course g) ... (grade-value g) ...))
 
(define EX-GRADE-1 (make-grade 1 "Fundies 1" 95))
(define EX-GRADE-2 (make-grade 1 "Psychoceramics" 65))
(define EX-GRADE-3 (make-grade 2 "Programming Languages" 85))
(define EX-GRADE-4 (make-grade 2 "Fundies 1" 75))
(define EX-GRADE-5 (make-grade 3 "Fundies 1" 68))
(define EX-GRADE-6 (make-grade 3 "Cybernetics" 82))
(define EX-GRADE-7 (make-grade 3 "Phonology" 89))
(define EX-GRADE-8 (make-grade 4 "Fundies 1" 55))
 
(define-struct student-grades [name grades])
; A StudentGrades is a (make-student-grades String [List-of Number]).
; Interpretation: (make-student-grades name grades) represents the grades
; that a student has received in all courses.
 
(define (student-grades-template sg)
  (... (student-grades-name sg) ...
       ; i.e., template for [List-of Number]
       (lon-template (student-grades-grades sg)) ...))
 
(define EX-STUDENT-GRADES-1 (make-student-grades "Alice" (list 95 65)))
(define EX-STUDENT-GRADES-2 (make-student-grades "Bob" (list 85 75)))
(define EX-STUDENT-GRADES-3 (make-student-grades "Carol" (list 68 82 89)))
; ========================================= END IMPORTS =========================================

(define EX-STUDENT-4 (make-student "David" 4))
(define EX-STUDENT-5 (make-student "Eve" 5))
(define EX-STUDENT-6 (make-student "Frank" 6))

(define EX-GRADE-9 (make-grade 4 "Fundies 1" 45))
(define EX-GRADE-10 (make-grade 5 "Fundies 1" 35))
(define EX-GRADE-11 (make-grade 6 "Fundies 1" 25))
(define EX-GRADE-12 (make-grade 4 "Psychoceramics" 15))
(define EX-GRADE-13 (make-grade 5 "Programming Languages" 5))
(define EX-GRADE-14 (make-grade 6 "Programming Languages" 95))
(define EX-GRADE-15 (make-grade 6 "Fundies 1" 15))

(define loS1 (list EX-STUDENT-1 EX-STUDENT-2 EX-STUDENT-3))
(define loG1 (list EX-GRADE-1 EX-GRADE-2 EX-GRADE-3 EX-GRADE-4 
                   EX-GRADE-5 EX-GRADE-6 EX-GRADE-7 EX-GRADE-8))

(define loS2 (list EX-STUDENT-4 EX-STUDENT-5 EX-STUDENT-6))
(define loG2 (list EX-GRADE-9 EX-GRADE-10 EX-GRADE-11 EX-GRADE-12 
                   EX-GRADE-13 EX-GRADE-14 EX-GRADE-15))

; Exercise 3
; students->student-grades : [List-of Student] [List-of Grade] -> [List-of StudentGrades]
; to produce a list of student grades from the given list of students and grades
(check-expect (students->student-grades loS1 loG1)
              (list EX-STUDENT-GRADES-1 EX-STUDENT-GRADES-2 EX-STUDENT-GRADES-3))
(check-expect (students->student-grades loS2 loG2)
              (list (make-student-grades "David" (list 45 15))
                    (make-student-grades "Eve" (list 35 5))
                    (make-student-grades "Frank" (list 25 95 15))))
(check-expect (students->student-grades loS1 '())
              (list (make-student-grades "Alice" '())
                    (make-student-grades "Bob" '())
                    (make-student-grades "Carol" '())))
(check-expect (students->student-grades '() loG1) '())


(define (students->student-grades los log)
  (map (lambda (s) (students->student-grades-helper s log)) los))

; students->student-grades-helper : Student [List-of Grade] -> StudentGrades
; to produce a student grades from the given student and grades
(check-expect (students->student-grades-helper EX-STUDENT-1 loG1) EX-STUDENT-GRADES-1)
(check-expect (students->student-grades-helper EX-STUDENT-2 loG1) EX-STUDENT-GRADES-2)
(check-expect (students->student-grades-helper EX-STUDENT-3 loG1) EX-STUDENT-GRADES-3)

(define (students->student-grades-helper s log)
  (local ([define (extract-grades nuid)
            (map grade-value (filter (lambda (g) (= (grade-nuid g) nuid)) log))])
    (make-student-grades (student-name s) (extract-grades (student-nuid s)))))
