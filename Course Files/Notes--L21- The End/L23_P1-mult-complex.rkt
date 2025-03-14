;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname L23_P1-mult-complex) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; LECTURE 23: MULTIPLE COMPLEX INPUTS
;
; MOTIVATION
; Thus far, our functions have generally accepted a single input. In cases where
; they've accepted multiple inputs, the second input has tended to be "simple".
; But what happens if you have two complex inputs that your function must process?
; 
; GENERAL APPROACHES
; Multiple complex inputs requires a bit more thinking and creativity than what
; we've been doing up to now. There are still patterns that we can detect, though.
; We will characterize a few common ways in which you could be dealing with
; multiple complex inputs:
; 
; 1. Using arguments sequentially. In other words, you could be looking only at
; the first complex input, and then dealing with the second.
; (Roughly, Case 1 in the book.)
;
; 2. Using arguments in parallel. In other words, you could be taking the
; two arguments apart in parallel. (Combines Cases 2/3 in the book.)
;
; 3. Using the cross-product of the arguments. In other words, for everything
; in the first argument, you're going to examine the second argument.
; (An interesting combination of the cases.)
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CASE 1: USING ARGUMENTS SEQUENTIALLY
; Some multiple-complex-input problems are actually simpler problems in
; disguise, where you're essentially using the two arguments iteratively.
; You follow the template for the first until you hit the base case, and
; then follow the template for the second.
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function append-lists that accepts two lists, and returns
; a list containing all of the elements of the first followed by all
; of the elements of the second.
; 
; Start with the first three steps of the design recipe:

; append-lists : [List-of Any] [List-of Any] -> [List-of Any]
; Appends two lists together
 
(check-expect (append-lists '() (list 1 2)) (list 1 2))
(check-expect (append-lists (list 1 2) '()) (list 1 2))
(check-expect (append-lists (list "a") (list 1 2)) (list "a" 1 2))
 
; Clearly, we want to deal with all of the first lists' elements, then
; deal with all of the second. We're ignoring the second list until
; we're done with the first. So, we'll start with the template for list1;

#;
(define (append-lists l1 l2)
  (cond [(empty? l1) ...]
        [(cons? l1) (... (first l1) ...
                         (append-lists (rest l1) l2) ...)]))

; Let's finish the "cons?" branch first
;<DO NOW><NODIFY ABOVE>


; But what do we put in the "[(empty? l1) ...]" clause?
; We've exhausted list1, so we just continue with list2:
#;
(define (append-lists l1 l2)
  (cond [(empty? l1) (cond [(empty? l2) ...]
                           [(cons? l2) (... (first l2) ...
                                             (append-lists l1 (rest l2)) ...)])]
        ; Adapt the template above
        ;<DO NOW><NODIFY ABOVE>
        
        [(cons? l1) (cons (first l1)
                          (append-lists (rest l1) l2))]))


; For this particular example, we can see that the nested cond of this function is
; a) completely ignoring l1 (after all, it's empty); and
; b) re-cons'ing all of the elements of l2 onto empty, which is just
;    creating a literal copy of l2.
; This is unnecessary: we could do:

(define (append-lists l1 l2)
  (cond [(empty? l1) l2]
        [(cons? l1) (cons (first l1)
                          (append-lists (rest l1) l2))]))
; Note that we can't just finesse the outer cond in the same way: yes, it is
; just cons'ing each element of l1 back on, but it is NOT onto an empty list.
; Also note that we can't just do "(list l1 l2)"--think about why!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CASE 2: USING ARGUMENTS IN PARALLEL
; In this pattern, you're using both arguments in parallel,
; taking a step with both lists as you recur.

; The book deals with two versions of this: one in which you assume they are
; the same length, the other in which you don't. We'll focus on the latter,
; since this is more general, and same-length lists can be considered just a
; special case of two-lists-of-any-length

; Recall this data design from earlier in the term:

; A NaturalNumber (Nat) is one of:
; - 0
; - (add1 Nat)
 
(define NAT-0 0)
(define NAT-1 (add1 NAT-0))
(define NAT-2 (add1 NAT-1))
 
(define (nat-temp nat)
  (...
   (cond [(zero? nat) ...]
         [(positive? nat) (... nat ...
                               (nat-temp (sub1 nat)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function get-element that accepts a list and a
; NaturalNumber and returns the element at the location in the list specified
; by the number. If there is no such element, it should raise an error.
; (Do we know how to "raise an error"? We will in a minute!!)

; Start with the first three steps of the design recipe:

; get-element : [List-of Any] Nat -> Any
; Gets the element in the specified location in the list,
; or raises an error if invalid
 
(check-error (get-element '() 0))
(check-expect (get-element (list 1 2 4) 0) 1)
(check-expect (get-element (list 1 2 4) 1) 2)
(check-expect (get-element (list 1 2 4) 2) 4)
(check-error (get-element (list 1 2 3) 4))

;<BRAINSTORM>
; We want to "walk down" the list AND the number, until either:
; a) the list is empty, in which case we raise an error; or
; b) the number is 0, in which case we return the current element.

; Here are the two relevant templates.
#;
(define (nat-temp nat)
  (...
   (cond [(zero? nat) ...]
         [(positive? nat) (... nat ...
                               (nat-temp (sub1 nat)) ...)])))
#;
(define (loa-temp loa)
  (...
   (cond [(empty? loa) ...]
         [(cons? loa) (... (first loa) ...
                           (loa-temp (rest loa)) ...)])))

; Nesting templates, as we did with Case 1, will not work here. Our approach
; will be to form a joint template, with all possible combinations:

(define (loa-nat-temp loa nat)
  (...
   (cond [(and (empty? loa) (zero? nat))     ...]
         [(and (empty? loa) (positive? nat)) (... nat ...)]
         [(and (cons? loa) (zero? nat))      (... (first loa) ... (rest loa) ...)]
         [(and (cons? loa) (positive? nat))  (... (first loa) ...
                                                  nat ...
                                                  (loa-nat-temp (rest loa) (sub1 nat)) ...)])))

; Let's use this template to create our function:
#;
(define (get-element loa nat)
  ; Adapt from template:
  #;
  (...
   (cond [(and (empty? loa) (zero? nat))     ...]
         [(and (empty? loa) (positive? nat)) (... nat ...)]
         [(and (cons? loa) (zero? nat))      (... (first loa) ... (rest loa) ...)]
         [(and (cons? loa) (positive? nat))  (... (first loa) ...
                                                  nat ...
                                                  (loa-nat-temp (rest loa) (sub1 nat)) ...)]))
  ;<DO NOW><SCROLL>







  
  (cond
    [(and (empty? loa) (zero? nat))     (error "Invalid index")]
    [(and (empty? loa) (positive? nat)) (error "Invalid index")]
    [(and (cons? loa) (zero? nat))      (first loa)]
    [(and (cons? loa) (positive? nat))  (get-element (rest loa) (sub1 nat))]))

; Can we simplify further?

(define (get-element loa nat)
  (cond
    [(empty? loa)                      (error "Invalid index")]
    [(and (cons? loa) (zero? nat))     (first loa)]
    [(and (cons? loa) (positive? nat)) (get-element (rest loa) (sub1 nat))]))

; This is useful enough that ISL has its own implmentation: list-ref :)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CASE 3: USING THE CROSS-PRODUCT OF THE ARGUMENTS

; This is the pattern where you're using the "cross-product" of the two lists,
; somewhat like you would in creating the cross-product of two matrices:
; processing each element of the first argument against all of the elements
; of the entire second argument.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise;
; Design the function intersect that takes two lists of numbers, and returns those elements
; of the first that appear in the second (in the order they appear in the first).

; Do the first three steps of the design recipe:

; intersect : [List-of Number] [List-of Number] -> [List-of Number]
; Returns the subset of the two lists that are common, in the order they appear in the first.
; (subtle difference from classic set intersection)

; IMPORTANT: what are good tests?
;<BRAINSTORM><SCROLL>








(check-expect (intersect/v1 (list 1 2 3) (list 3 1 4 5 6)) (list 1 3))
(check-expect (intersect/v1 (list 1 2 3) (list 4 5 6)) '())
(check-expect (intersect/v1 (list 1 2 1) (list 4 1 6)) (list 1 1))
(check-expect (intersect/v1 (list 1 2 3) (list)) '())
(check-expect (intersect/v1 (list) (list 4 5 6)) '())
(check-expect (intersect/v1 (list 3 2 1) (list 1 4 2 5 3 6)) (list 3 2 1))

;<BRAINSTORM>








; We're effectively walking over the first list, doing something on each element.
; Perfect justification for a helper function to do what we want on the second list.
#;
(define (intersect/v1 l1 l2)
  (...
   (cond [(empty? l1) ...]
         [(cons? l1) (... (l2-temp (first l1) l2) ...
                          (intersect/v1 (rest l1) l2) ...)])))
#;
(define (l2-temp x l)
  (...
   (cond [(empty? l) ...]
         [(cons? l) (... x ...
                         (first l) ...
                         (l2-temp x (rest l)) ...)])))

; Let's flesh this out:
(define (intersect/v1 l1 l2)
  ; Start from template
  #;
  (cond [(empty? l1) ...]
        [(cons? l1) (... (l2-temp (first l1) l2) ...
                         (intersect/v1 (rest l1) l2) ...)])
  ;<DO NOW><SCROLL>







  
  (cond [(empty? l1) '()]
        [(cons? l1) (if (num-exists-in-list? (first l1) l2)
                        (cons (first l1)
                              (intersect/v1 (rest l1) l2))
                        (intersect/v1 (rest l1) l2))]))

; num-exists-in-list? : Number [List-of Number] -> Boolean
; Returns whether the number can be found in the list
 
(check-expect (num-exists-in-list? 1 (list 1 2 3)) #true)
(check-expect (num-exists-in-list? 47 (list 1 2 3)) #false)
(check-expect (num-exists-in-list? 1 '()) #false)

(define (num-exists-in-list? x l)
  ; Start from template
  #;
  (cond [(empty? l) ...]
         [(cons? l) (... x ...
                         (first l) ...
                         (num-exists-in-list? x (rest l)) ...)])
  ;<DO NOW><SCROLL>







  
  (cond [(empty? l) #false]
        [(cons? l) (or (= x (first l))
                       (num-exists-in-list? x (rest l)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Can we make this more general? Let's abstract it to work for
; any kind of list. What needs to be parameterized?
;<BRAINSTORM>








; intersect : (X) [List-of X] [List-of X] [X X -> Boolean] -> [List-of X]
; Returns the subset of the two lists that are common given an equality relation
 
(check-expect (intersect (list 1 2 3) (list 3 1 4 5 6) =) (list 1 3))
(check-expect (intersect (list 1 2 3) (list 4 5 6) =) '())
(check-expect (intersect (list 1 2 1) (list 4 1 6) =) (list 1 1))
(check-expect (intersect (list 1 2 3) (list) =) '())
(check-expect (intersect (list) (list 4 5 6) =) '())
(check-expect (intersect (list 3 2 1) (list 1 4 2 5 3 6) =) (list 3 2 1))
(check-expect (intersect (list "a" "b" "c") (list "c" "d" "a") string=?) (list "a" "c"))
(check-expect (intersect (list "a" "b" "c") (list "c" "d" "a" "a") string=?) (list "a" "c"))
;<DO NOW><START W/COPY FROM ABOVE><SCROLL>










#;
(define (intersect l1 l2 same?)
  (cond [(empty? l1) '()]
        [(cons? l1) (if (exists-in-list? (first l1) l2 same?)
                        (cons (first l1)
                              (intersect (rest l1) l2 same?))
                        (intersect (rest l1) l2 same?))]))
#;
(define (exists-in-list? x l same?)
  (cond [(empty? l) #false]
        [(cons? l) (or (same? x (first l))
                       (exists-in-list? x (rest l) same?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's see if we can improve things with local.
; First, let's move the helper inside.
;<DO NOW><MODIFY ABOVE><SCROLL>










#;
(define (intersect l1 l2 same?)
  (local [; exists-in-list? : (X) X [List-of X] -> Boolean
          ; returns true if the element exists in l
          ; (exists-in-list? 1 (list 1 2 3) =) is #true
          ; (exists-in-list? 47 (list 1 2 3) =) is #false
          ; (exists-in-list? "a" (list "a" "b") string=?) is #true
          (define (exists-in-list? x l)
            (cond [(empty? l) #false]
                  [(cons? l) (or (same? x (first l))
                                 (exists-in-list? x (rest l)))]))]
    (cond [(empty? l1) '()]
          [(cons? l1) (if (exists-in-list? (first l1) l2)
                          (cons (first l1)
                                (intersect (rest l1) l2))
                          (intersect (rest l1) l2 same?))])))

; ...and more local optimization
;<DO NOW><MODIFY ABOVE><SCROLL> helper: (consider-each <list>)










(define (intersect l1 l2 same?)
  (local [; consider-each : [List-of X] -> [List-of X]
          ; keeps only those in the list that appear in l2
          (define (consider-each l)
            (cond [(empty? l) '()]
                  [(cons? l) (if (exists-in-list? (first l) l2)
                                 (cons (first l) (consider-each (rest l)))
                                 (consider-each (rest l)))]))
 
          ; exists-in-list? : (X) X [List-of X] -> Boolean
          ; returns true if the element exists in l
          (define (exists-in-list? x l)
            (cond [(empty? l) #false]
                  [(cons? l) (or (same? x (first l))
                                 (exists-in-list? x (rest l)))]))]
    (consider-each l1)))
; Let's see if we can't simplify by taking advantage of list abstractions.
;<BRAINSTORM>








; Observe that consider-each is essentially a filter and exists-in-list?
; is essentially an ormap. You could do:

(define (intersect/abst l1 l2 same?)
  (local [; exists-in-list? : X [List-of X] -> Boolean
          ; returns true if the element exists in l
          (define (exists-in-list? x l)
            (ormap (λ (x2) (same? x x2)) l))]
    (filter (λ (x1) (exists-in-list? x1 l2)) l1)))

; Test to make sure!
(check-expect (intersect/abst (list 1 2 3) (list 4 5 6) =) '())
(check-expect (intersect/abst (list 1 2 1) (list 4 1 6) =) (list 1 1))
(check-expect (intersect/abst (list 1 2 3) (list) =) '())
(check-expect (intersect/abst (list) (list 4 5 6) =) '())
(check-expect (intersect/abst (list 3 2 1) (list 1 4 2 5 3 6) =) (list 3 2 1))
(check-expect (intersect/abst (list "a" "b" "c") (list "c" "d" "a") string=?) (list "a" "c"))
(check-expect (intersect/abst (list "a" "b" "c") (list "c" "d" "a" "a") string=?) (list "a" "c"))

