#lang racket

(require 2htdp/image)


;; Exercise 5
(define (binomial a b)
  (sqr (+ a b)))

(binomial 1 2)

;; Exercise 8
(define (alternative_names first_name last_name)
  (string-append "’" first_name " " last_name "’ or ’" last_name ", " first_name "’"))


;; Exercise 10
(define MY-FNAME "Vignesh")
(define MY-LNAME "Saravanakumar")
(alternative_names MY-FNAME MY-LNAME)

(define MY-FNAME_2 "Eli")
(define MY-LNAME_2 "Beber")
(alternative_names MY-FNAME_2 MY-LNAME_2)

(define MY-FNAME_3 "Mary Louise")
(define MY-LNAME_3 "Smith")
(alternative_names MY-FNAME_3 MY-LNAME_3)


;; Exercise 13
(define SQR1 (rectangle 30 30 "solid" "black"))
(define SQR2 (rectangle 29 29 "outline" "black"))

;; Exercise 14
(above (beside SQR1 SQR2 SQR1)
(beside SQR2 SQR1 SQR2)
(beside SQR1 SQR2 SQR1))

