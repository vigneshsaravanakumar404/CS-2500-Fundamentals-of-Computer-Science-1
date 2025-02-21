#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define LIST-1 (cons 1 (cons 2 (cons 3 '()))))
(define LIST-2 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
(define LIST-3 (cons 43 (cons 70 (cons 12 '()))))


; keep-greater-than : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers greater or equal to the given number
(check-expect (keep-greater-than LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than LIST-3 50) (cons 70 '()))

(define (keep-greater-than lon num)
  (cond
    [(empty? lon) '()]
    [(>= (first lon) num) (cons (first lon) (keep-greater-than (rest lon) num))]
    [else (keep-greater-than (rest lon) num)]))


; keep-less-than : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers less than the given number
(check-expect (keep-less-than LIST-1 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than LIST-2 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than lon num)
  (cond
    [(empty? lon) '()]
    [(<= (first lon) num) (cons (first lon) (keep-less-than (rest lon) num))]
    [else (keep-less-than (rest lon) num)]))


; keep-select: [List-of Number] Number Number -> [List-of Number]
; Produces a list of numbers that satisfy the given modifier
(check-expect (keep-select LIST-1 2 1000) (cons 2 (cons 3 '())))
(check-expect (keep-select LIST-2 0 2) (cons 1 (cons 2 '())))
(check-expect (keep-select LIST-3 70 70) (cons 70 '()))

(define (keep-select lon low high)
  (cond 
    [(empty? lon) '()]
    [(and (>= (first lon) low) (<= (first lon) high)) 
     (cons (first lon) (keep-select (rest lon) low high))]
    [else (keep-select (rest lon) low high)]))


; keep-greater-than/v2 : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers greater or equal to the given number
(check-expect (keep-greater-than/v2 LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than/v2 LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than/v2 LIST-3 50) (cons 70 '()))

(define (keep-greater-than/v2 lon num)
  (keep-select lon num 1000))

; keep-less-than/v2 : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers less than the given number
(check-expect (keep-less-than/v2 LIST-1 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than/v2 LIST-2 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than/v2 LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than/v2 lon num)
  (keep-select lon 0 num))