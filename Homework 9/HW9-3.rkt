;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW9-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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

(define loS1 (list EX-STUDENT-1 EX-STUDENT-2 EX-STUDENT-3))
(define loG1 (list EX-GRADE-1 EX-GRADE-2 EX-GRADE-3 EX-GRADE-4 EX-GRADE-5 EX-GRADE-6 EX-GRADE-7 EX-GRADE-8))
(define loSG1 (list EX-STUDENT-GRADES-1 EX-STUDENT-GRADES-2 EX-STUDENT-GRADES-3))


; Exercise 3
; students->student-grades : [List-of Student] [List-of Grade] -> [List-of StudentGrades]
; to produce a list of student grades from the given list of students and grades
(check-expect (students->student-grades loS1 loG1)
              (list EX-STUDENT-GRADES-1 EX-STUDENT-GRADES-2 EX-STUDENT-GRADES-3))


(define (students->student-grades los log)
  (map (lambda (s) (students->student-grades-helper s log)) los))

(define (students->student-grades-helper s log)
  (local [(define (extract-grades nuid log)
            (map grade-value (filter (lambda (g) (= (grade-nuid g) nuid)) log)))]
    (make-student-grades (student-name s) (extract-grades (student-nuid s) log))))

