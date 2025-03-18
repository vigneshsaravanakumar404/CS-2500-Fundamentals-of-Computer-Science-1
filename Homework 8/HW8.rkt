;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

; ====================================================================================
; Import Functions: COPIED DIRECTLY FROM HW6-DISTRIB.RKT
; Design recipes are not included for this section because it is copied 
; ====================================================================================
(define (count-trues lob)
  (cond [(empty? lob) 0]
        [(cons? lob) (if (first lob)
                           (add1 (count-trues (rest lob)))
                           (count-trues (rest lob)))]))


(define (first-true lob)
  (cond [(empty? lob) -1]
        [(cons? lob) (if (first lob)
                          0
                          (if (= -1 (first-true (rest lob)))
                              -1
                              (add1 (first-true (rest lob)))))]))


(define (set-true lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #true (rest lob))
                          (cons (first lob) (set-true (rest lob) (sub1 n))))]))


(define (set-false lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #f (rest lob))
                          (cons (first lob) (set-false (rest lob) (sub1 n))))]))

; ====================================================================================
; End of Import Functions
; ====================================================================================



; ====================================================================================
; Import Functions: COPIED DIRECTLY MY SUBMISSION FOR HW6
; ====================================================================================

; count-total : [List-of Boolean] -> NonNegInteger
; Produces the total number of values in the list
;;; (check-expect (count-total (cons #true (cons #f (cons #true '())))) 3)
;;; (check-expect (count-total (cons #f (cons #f (cons #true (cons #true '()))))) 4)
;;; (check-expect (count-total (cons #true (cons #true (cons #f '())))) 3)

;;; (define (count-total lob)
;;;   (cond
;;;     [(empty? lob) 0]
;;;     [else (+ 1 (count-total (rest lob)))]))

; ====================================================================================
; End of Import Functions
; ====================================================================================


; DATA DEFINITIONS
(define HBS-1
  (list (list #t #t)
        (list #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)))

(define HBS-2
  (list (list #t #f)
        (list #f #f #t #f)
        (list #f #f #f #f #f #f #f #f)))

(define HBS-3
  (list (list #t #f)
        (list #f #f #t #f)
        (list #f #f #f #f #f #f #f #t)))

(define HBS-4
  (list (list #f #t)
        (list #f #t #f #f)
        (list #f #t #f #f #f #f #f #f)))
    
(define HBS-5
  (list (list #f #f)
        (list #f #f #f #f)
        (list #f #f #f #f #f #f #f #f)))

(define HBS-6
  (list
   (list #t #f)
   (list #f #f #t #f)
   (list #f #f #f #f #f #f #t #f)))

(define HBS-7
  (list
   (list #f #f)
   (list #f #f #f #f) 
   (list #f #f #f #f #f #f #f #f)))

(define HBS-8
  (list
   (list #t #t)
   (list #f #f #f #f)
   (list #f #f #f #f #f #f #f #f)))

(define HBS-9
  (list (list #f #f) 
         (list #f #true #true #f) 
         (list #f #f #f #f #f #f #f #true) 
         (list #f #true #true #f #f #f #f #f #f #f #f #f #f #f #f #f)))

; A HierarchicalBitmapSet (HBS) is a [List-of [List-of Boolean]]

(define-struct hbs-alloc [block hbs])
; A HBSAllocResult is a (make-hbs-alloc Integer HBS)
; Represents the result of an allocation call, where:
; - block is the starting block number for the chunk that was allocated;
;   or -1 if no space available
; - hbs is the updated HBS after the allocation

(define HBSAR-1 (make-hbs-alloc 2
                                (list (list #f #t)
                                      (list #t #f #f #f))))
(define HBSAR-2 (make-hbs-alloc 0
                                (list (list #t #t)
                                      (list #f #f #f #f))))
(define HBSAR-3 (make-hbs-alloc -1
                                (list (list #t #t)
                                      (list #f #f #f #f))))

(define (hbsar-temp hbsar)
  (... (hbs-alloc-block hbsar) ...
       (hbs-temp (hbs-alloc-hbs hbsar) ...)))



; Exercise 1a
; blocks-remaining : HBS -> NonNegInteger
; Produces the number of blocks remaining in the list of blocks
(check-expect (blocks-remaining HBS-1) 8)
(check-expect (blocks-remaining HBS-3) 7)
(check-expect (blocks-remaining HBS-2) 6)
(check-expect (blocks-remaining HBS-4) 7)

(define (blocks-remaining HBS)
  (cond
    [(empty? HBS) 0]
    [else (+ (* (expt 2 (- (length HBS) 1)) (count-trues (first HBS)))
             (blocks-remaining (rest HBS)))]))


; Exercise 1b
; find-chunk : HBS NonNegInteger -> NonNegInteger
; produce the index of the first block of a free chunk of the requested size
(check-expect (find-chunk HBS-4 1) 1)
(check-expect (find-chunk HBS-4 2) 2)
(check-expect (find-chunk HBS-4 4) 4)
(check-expect (find-chunk HBS-4 8) -1)
(check-expect (find-chunk HBS-5 1) -1)
(check-expect (find-chunk HBS-5 2) -1)
(check-expect (find-chunk HBS-5 4) -1)
(check-expect (find-chunk HBS-6 1) 6)
(check-expect (find-chunk HBS-7 4) -1)
(check-expect (find-chunk HBS-8 4) 0)


(define (find-chunk HBS s)
  (local [(define expected (expt 2 (- (length HBS) 1)))]
    (cond
      [(empty? HBS) -1]
      [(and (= s expected) (> (count-trues (first HBS)) 0)) 
       (* (first-true (first HBS)) expected)]
      [else (find-chunk (rest HBS) s)])))
    

; Exercise 1c
; initialize-hbs : [List-of Boolean] -> HBS
; consumes a single bitmap of the free blocks on a drive, and produces the corresponding HBS 
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #t)) HBS-1)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #f #f)) HBS-2)
(check-expect (initialize-hbs (list #f #f #f #f #f #f #f #f)) HBS-5)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #f)) HBS-6)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #t)) HBS-8)

(define (initialize-hbs lob)
  (local [(define (r lob o)
            (cond
              [(= (length lob) 2) (cons lob o)]
              [else (r (pairwise-and lob) (cons (coalesce lob) o))]))]
    (r lob '())))

; pairwise-and : [List-of Boolean] -> [List-of Boolean]
; consumes a list of booleans and produces a list of booleans 1/2 the size of the input list 
; representing the level above
(check-expect (pairwise-and (list #t #t #f #f)) (list #t #f))
(check-expect (pairwise-and (list #t #t #t #t)) (list #t #t))
(check-expect (pairwise-and (list #f #f #f #f)) (list #f #f))
(check-expect (pairwise-and (list #t #f #t #f)) (list #f #f))

(define (pairwise-and lst)
  (cond
    [(empty? lst) '()]
    [(empty? (rest lst)) '()]
    [else (cons (and (first lst) (second lst)) (pairwise-and (rest (rest lst))))]))

; coalesce : [List-of Boolean] -> [List-of Boolean]
; consumes a list of booleans and produces a list of booleans with every pair of booleans coalesced
; where adjacent trues are replaced with false
(check-expect (coalesce (list #t #t #f #f)) (list #f #f #f #f))
(check-expect (coalesce (list #t #t #t #t)) (list #f #f #f #f))
(check-expect (coalesce (list #f #f #f #f)) (list #f #f #f #f))
(check-expect (coalesce (list #t #f #t #f)) (list #t #f #t #f))
(check-expect (coalesce (list #t #f #t #t)) (list #t #f #f #f))

(define (coalesce  lst)
    (cond
      [(empty? lst) '()]
      [(and (first lst) (second lst)) (cons #f (cons #f (coalesce (rest (rest lst)))))]
      [else (cons (first lst) (cons (second lst) (coalesce  (rest (rest lst)))))]))


; Exercise 2
; alloc-chunk : HBS NonNegInteger -> HBS
; consumes a HBS and a size s, and produces a new HBS with the first free chunk of size s allocated
;;; (check-expect (alloc-chunk (list (list #f #t) (list #f #t #f #f) (list #f #t #f #f #f #f #f #f)) 2)
;;;               (list (list #f #t) (list #f #f #f #f) (list #f #t #f #f #f #f #f #f)))


;;; (define (alloc-chunk HBS s)
;;;   (cond
;;;     [(empty? (rest HBS)) (initialize-hbs (set-false-range (last HBS) (find-chunk HBS s) s))]
;;;     []))

;;; ; 

;;; (define (set-false-range lst start count)
;;;   (cond
;;;     [(empty? lst) '()]  ; Base case: empty list
;;;     [(zero? count) lst]  ; Stop modifying once count reaches zero
;;;     [(= start 0) (cons #f (set-false-range (rest lst) 0 (- count 1)))]  ; Start setting #t
;;;     [else (cons (first lst) (set-false-range (rest lst) (- start 1) count))]))  ; Decrement start

;;; (alloc-chunk (list (list #f #t) (list #f #t #f #f) (list #f #t #f #f #f #f #f #f)) 2)