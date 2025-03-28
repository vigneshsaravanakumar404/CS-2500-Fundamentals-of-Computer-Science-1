;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Lab 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct bnode [value left right])
; A [BinTree-of X] is one of
; - #false
; - (make-bnode X [BinTree-of X] [BinTree-of X])
; Interpretation: A value and left and right subtrees,
; or a sentinel value representing an empty tree
; Examples:
(define BT-0 #false)
(define BT-1 (make-bnode 10
                         (make-bnode 4
                                     (make-bnode 5 #false #false)
                                     #false)
                         (make-bnode 3
                                     (make-bnode 2 #false #false)
                                     (make-bnode 9 #false #false))))
(define BT-2 (make-bnode "College"
                         (make-bnode "Khoury" #false #false)
                         (make-bnode "Computer"
                                     (make-bnode "of" #false #false)
                                     (make-bnode "Sciences" #false #false))))
 
(define (bt-temp bt)
  (...
    (cond [(boolean? bt) ...]
          [(bnode? bt) (... (bnode-value bt) ...
                            (bt-temp (bnode-left bt)) ...
                            (bt-temp (bnode-right bt)) ...)])))

; Exercise 1
; bt-height : [BinTree-of X] -> Number
; Maximum depth of binary tree
(check-expect (bt-height BT-0) 0)
(check-expect (bt-height BT-1) 3)
(check-expect (bt-height BT-2) 3)

(define (bt-height bn)
  (cond
    [(boolean? bn) 0]
    [else (max (add1 (bt-height (bnode-left bn))) (add1 (bt-height (bnode-right bn))))]))

; Exercise 2
; bt-flatten : [BinTree-of X] -> [List-of X]
; to produce a list of the values in the tree in in-order
(check-expect (bt-flatten BT-2) (list "Khoury" "College" "of" "Computer" "Sciences"))
(check-expect (bt-flatten BT-1) (list 5 4 10 2 3 9))
(check-expect (bt-flatten BT-0) empty)

(define (bt-flatten bt)
  (cond [(boolean? bt) empty]
        [(bnode? bt) (append (bt-flatten (bnode-left bt))
                             (list (bnode-value bt))
                             (bt-flatten (bnode-right bt)))]))

; Exercise 3
; bt-map : (X) [X -> Y] [BinTree-of X] -> [BinTree-of Y]
; takes a function and a binary tree and applies the function to each value in the tree
(check-expect (bt-map add1 BT-1)
              (make-bnode 11
                          (make-bnode 5
                                      (make-bnode 6 #false #false)
                                      #false)
                          (make-bnode 4
                                      (make-bnode 3 #false #false)
                                      (make-bnode 10 #false #false))))

(check-expect (bt-map string-append BT-2)
              (make-bnode "College"
                          (make-bnode "Khoury" #false #false)
                          (make-bnode "Computer"
                                      (make-bnode "of" #false #false)
                                      (make-bnode "Sciences" #false #false))))

(check-expect (bt-map add1 BT-0) #false)
(check-expect (bt-map add1 (make-bnode 1 #false #false)) (make-bnode 2 #false #false))

(define (bt-map f bt)
  (cond [(boolean? bt) #false]
        [(bnode? bt) (make-bnode (f (bnode-value bt))
                                 (bt-map f (bnode-left bt))
                                 (bt-map f (bnode-right bt)))]))

; Exercise 4
; bt-fold : (X Y Z) [Y X Y -> Y] Y [BinTree-of X] -> Y
; takes a function, a base case value, and a binary tree and 
; applies the function to each value in the tree
(check-expect (bt-fold string-append "" BT-2) "CollegeKhouryComputerofSciences")
(check-expect (bt-fold + 0 BT-1) 33)
(check-expect (bt-fold + 0 BT-0) 0)

(define (bt-fold combine base bt)
  (cond
    [(boolean? bt) base]  ; If the tree is empty (#false), return base case value
    [(bnode? bt) 
     (combine (bnode-value bt)   
              (bt-fold combine base (bnode-left bt))
              (bt-fold combine base (bnode-right bt)))]))


; Exercise 5

(check-expect (bt-height/v2 BT-0) 0)
(check-expect (bt-height/v2 BT-1) 3)
(check-expect (bt-height/v2 BT-2) 3)

(define (bt-height/v2 bt)
  (bt-fold (lambda (v l r) (max (add1 l) (add1 r))) 0 bt))


; Exercise 6
; bt-flatten/v2 : [BinTree-of X] -> [List-of X]
; to produce a list of the values in the tree in in-order using bt-fold
(check-expect (bt-flatten/v2 BT-2) (list "Khoury" "College" "of" "Computer" "Sciences"))
(check-expect (bt-flatten/v2 BT-1) (list 5 4 10 2 3 9))
(check-expect (bt-flatten/v2 BT-0) empty)

(define (bt-flatten/v2 bt)
  (bt-fold (lambda (v l r) (append l (list v) r)) empty bt))



; Exercise 7
; bt-map/v2 : (X) [X -> Y] [BinTree-of X] -> [BinTree-of Y]
; to produce a binary tree with the values in the tree transformed by f using bt-fold
(check-expect (bt-map/v2 add1 BT-1)
              (make-bnode 11
                          (make-bnode 5
                                      (make-bnode 6 #false #false)
                                      #false)
                          (make-bnode 4
                                      (make-bnode 3 #false #false)
                                      (make-bnode 10 #false #false))))

(check-expect (bt-map/v2 string-append BT-2)
              (make-bnode "College"
                          (make-bnode "Khoury" #false #false)
                          (make-bnode "Computer"
                                      (make-bnode "of" #false #false)
                                      (make-bnode "Sciences" #false #false))))

(check-expect (bt-map/v2 add1 BT-0) #false)
(check-expect (bt-map/v2 add1 (make-bnode 1 #false #false)) (make-bnode 2 #false #false))

(define (bt-map/v2 f bt)
  (bt-fold (lambda (v l r) (make-bnode (f v) l r)) #false bt))
