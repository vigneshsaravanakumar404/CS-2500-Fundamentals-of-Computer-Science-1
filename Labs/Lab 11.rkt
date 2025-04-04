;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Lab 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



; A PB is one of:
;-- '()
;-- (cons String (cons Number PB))
; interpretation A phone book such as
; (cons "Alan" (cons 617738.1212 pb))
; means "Alan"'s phone number is 617738.1212,
; and the rest of the phone book is pb

; Exercise 1
; remove-both : String PB -> PB
; removes all entries for the given name from the phone book
(check-expect (remove-both "Alan" (cons "Alan" (cons 1 (cons "Bob" (cons 2 empty))))) (cons "Bob" (cons 2 empty)))
(check-expect (remove-both "Bob" (cons "Alan" (cons 1 (cons "Bob" (cons 2 empty))))) (cons "Alan" (cons 1 empty)))
(check-expect (remove-both "Alan" (cons "Alan" (cons 1 (cons "Alan" (cons 2 (cons "Alan" (cons 3 empty))))))) empty)

(define (remove-both n pb)
  (cond
    [(empty? pb) empty]
    [(string=? n (first pb)) (remove-both n (rest (rest pb)))]
    [else (cons (first pb) (cons (second pb) (remove-both n (rest (rest pb)))))]))

; Exercise 2b
; drop : [List-of Posn] -> [List-of Posn]
; drops posns with y > 200, otherwise keeps y + 3
(check-expect (drop (list (make-posn 1 200) (make-posn 2 201))) (list (make-posn 1 203)))
(check-expect (drop (list (make-posn 1 200) (make-posn 2 199))) (list (make-posn 1 203) (make-posn 2 202)))
(check-expect (drop (list (make-posn 1 210) (make-posn 2 201))) empty)

(define (drop lst)
  (map (lambda (p) (make-posn (posn-x p) (+ 3 (posn-y p)))) (filter (lambda (p) (<= (posn-y p) 200)) lst)))


; Exercise 2c
; drop : [List-of Posn] -> [List-of Posn]
; drops posns with y > 200, otherwise keeps y + 3
(check-expect (drop-no-abstract (list (make-posn 1 200) (make-posn 2 201))) (list (make-posn 1 203)))
(check-expect (drop-no-abstract (list (make-posn 1 200) (make-posn 2 199))) (list (make-posn 1 203) (make-posn 2 202)))
(check-expect (drop-no-abstract (list (make-posn 1 210) (make-posn 2 201))) empty)

(define (drop-no-abstract lst)
  (cond
    [(empty? lst) empty]
    [(> (posn-y (first lst)) 200) (drop (rest lst))]
    [else (cons (make-posn (posn-x (first lst)) (+ 3 (posn-y (first lst)))) (drop (rest lst)))]))


 ; A Butterfly is one of:
 ;-- "body"
 ;-- (list LEFT-WING Butterfly RIGHT-WING)
(define LEFT-WING "(")
(define RIGHT-WING ")")

; Exercise 3
; count-wings : Butterfly -> NonNegInt
; counts the number of wings in a butterfly
(check-expect (count-wings "body") 0)
(check-expect (count-wings (list LEFT-WING "body" RIGHT-WING)) 2)
(check-expect (count-wings (list LEFT-WING (list LEFT-WING "body" RIGHT-WING) RIGHT-WING)) 4)

(define (count-wings b)
  (cond
    [(string? b) 0]
    [(cons? b) (+ (count-wings (second b)) 2)]))

; An Enumeration is one of:
;-- (make-bullets LoI) ;; bulletized items
;-- (make-points LoI) ;; numbered points
;
; An LoI is a [List-of Item]
;
; An Item is a structure: (make-item LoW)
;
; An LoW is a [List-of Word]
;
; A Word is one of:
;-- String
;-- Enumeration
;
; interpretation An Enumeration is a generic data
; representation of HTML, LateX, etc nested,
; itemized lists

(define-struct bullets (loi))
(define-struct points (loi))
(define-struct item (low))

(define BAD-STRING "@#$%")

; Exercise 4
; cleanse : Enumeration -> Enumeration
; removes all words with string that are equal to "@#$%"
(check-expect (cleanse (make-bullets (list (make-item (list "Hello" "@#$%")) (make-item (list (make-points (list (make-item (list "Nested" "@#$%")) (make-item (list "@#$%"))))))))) (make-bullets (list (make-item (list "Hello")) (make-item (list (make-points (list (make-item (list "Nested")))))))))


(define (cleanse enum)
  (cond
    [(bullets? enum)
     (make-bullets (cleanse-loi (bullets-loi enum)))]
    [(points? enum)
     (make-points (cleanse-loi (points-loi enum)))]))

; cleanse-loi : [List-of Item] -> [List-of Item]
(define (cleanse-loi loi)
  (map cleanse-item (filter (lambda (item)
                              (not (empty-low? (cleanse-item item))))
                            loi)))

; cleanse-item : Item -> Item
(define (cleanse-item itm)
  (make-item (cleanse-low (item-low itm))))

; empty-low? : Item -> Boolean
; Check if an item's low (list of words) is empty after cleansing
(define (empty-low? itm)
  (empty? (item-low itm)))

; cleanse-low : [List-of Word] -> [List-of Word]
(define (cleanse-low low)
  (foldr (lambda (w acc)
           (cond
             [(string? w)
              (if (string=? w BAD-STRING) acc (cons w acc))]
             [(or (bullets? w) (points? w))
              (let ([cleaned (cleanse w)])
                (if (empty-enum? cleaned) acc (cons cleaned acc)))]))
         '()
         low))

; empty-enum? : Enumeration -> Boolean
; Check if an enumeration is empty (has no items)
(define (empty-enum? enum)
  (cond
    [(bullets? enum) (empty? (bullets-loi enum))]
    [(points? enum) (empty? (points-loi enum))]))
