;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; Exercise 1a
; diag-matrix : number number -> (listof (listof number))
; to produce a diagonal matrix of size n with d on the diagonal
(check-expect (diag-matrix 3 1) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (diag-matrix 4 2) (list (list 2 0 0 0) (list 0 2 0 0) (list 0 0 2 0) (list 0 0 0 2)))
(check-expect (diag-matrix 4 3) (list (list 3 0 0 0) (list 0 3 0 0) (list 0 0 3 0) (list 0 0 0 3)))

(define (diag-matrix n d)
  (local [(define (row i)
            (local [(define (col j)
                      (if (= i j) d 0))]
              (build-list n col)))]
    (build-list n row)))

; Exercise 1b
; append-no-dups : [List-of X] [List-of X] (X) [X X -> Boolean] -> [List-of X]
; to produce a list of elements from the first list followed by the second list with no duplicates
; note that this implementation drops the first occurrence of a duplicate element not the last
(check-expect (append-no-dups (list 1 2 2 3) (list 3 4 5) =) (list 1 2 3 4 5))
(check-expect (append-no-dups (list 1 2 3 3 3) empty =) (list 1 2 3))
(check-expect (append-no-dups empty (list 1 2 3 3 3) =) (list 1 2 3))
(check-expect (append-no-dups (list 1 2 3 4 5) (list 6 7 99) =) (list 1 2 3 4 5 6 7 99))
(check-expect (append-no-dups (list "a" "b" "c" "b") (list "d" "b" "e") string=?) (list "a" "c" "d" "b" "e"))
(check-expect (append-no-dups (list 1 2 3 3 3) (list 1 2 333) =) (list 3 1 2 333))
(check-expect (append-no-dups (list #true #false) (list #false #true) boolean=?) (list #false #true))


(define (append-no-dups l1 l2 f)
  (local [(define (remove-duplicates l f)
            (cond
              [(empty? l) empty]
              [(is-duplicate? (first l) (rest l)) (remove-duplicates (rest l) f)]
              [else (cons (first l) (remove-duplicates (rest l) f))]))
          (define (is-duplicate? x lst)
            (cond
              [(empty? lst) #false]
              [(f x (first lst)) #true]  
              [else (is-duplicate? x (rest lst))]))]
    (remove-duplicates (append l1 l2) f)))


;! TwoList design recipe
; TwoList is one of:
; - '()
; - (cons (list X Y) TwoList)
; Interpretation a list of lists where each sublist contains the ith element of the first list
; and the ith element of the second list


; Exercise 2a
; zip : [List-of X] [List-of Y] -> TwoList
; to produce a list of lists where each sublist contains the ith element of the first list 
; and the ith element of the second list
(check-expect (zip (list 1 3 5 7) (list 2 4 6)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list 1 3 5) (list 2 4 6 8)) (list (list 1 2) (list 3 4) (list 5 6)))
(check-expect (zip (list 1 3 5 7) (list 2 4 6 8)) (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
(check-expect (zip '() (list 2 4 6 8)) '())
(check-expect (zip (list 1 3 5 7) '()) '())
(check-expect (zip '()'()) '())


(define (zip l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2)))]))


; Exercise 2b
;    : [List-of X] [List-of Y] (X Y -> Z) -> [List-of Z]
; to produce a list of the results of applying the function f to the ith element of the first list
(check-expect (map-2list + (list 1 2 3) (list 4 5 6)) (list 5 7 9))
(check-expect (map-2list * (list 1 2 3) (list 4 5 6)) (list 4 10 18))
(check-expect (map-2list - (list 1 2 3) (list 4 5 6)) (list -3 -3 -3))
(check-error (map-2list + (list 1 2 3) (list 4 5)) "Lists of unequal size")
(check-error (map-2list + (list 1 2) (list 4 5 6)) "Lists of unequal size")

(define (map-2list func l1 l2)
    (if (= (length l1) (length l2))
        (cond
          [(empty? l1) empty]
          [else (cons (func (first l1) (first l2)) (map-2list func (rest l1) (rest l2)))])
        (error "Lists of unequal size")))



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

