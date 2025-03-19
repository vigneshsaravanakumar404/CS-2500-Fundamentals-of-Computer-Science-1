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
                           (+ 1 (count-trues (rest lob)))
                           (count-trues (rest lob)))]))


(define (first-true lob)
  (cond [(empty? lob) -1]
        [(cons? lob) (if (first lob)
                          0
                          (if (= -1 (first-true (rest lob)))
                              -1
                              (+ 1 (first-true (rest lob)))))]))


(define (set-true lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #t (rest lob))
                          (cons (first lob) (set-true (rest lob) (- n 1))))]))


(define (set-false lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #f (rest lob))
                          (cons (first lob) (set-false (rest lob) (- n 1))))]))

; ====================================================================================
; End of Import Functions
; ====================================================================================



; ====================================================================================
; Import Functions: COPIED DIRECTLY MY SUBMISSION FOR HW6
; ====================================================================================

; count-total : [List-of Boolean] -> NonNegInteger
; Produces the total number of values in the list
;;; (check-expect (count-total (cons #t (cons #f (cons #t '())))) 3)
;;; (check-expect (count-total (cons #f (cons #f (cons #t (cons #t '()))))) 4)
;;; (check-expect (count-total (cons #t (cons #t (cons #f '())))) 3)

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
         (list #f #t #t #f) 
         (list #f #f #f #f #f #f #f #t) 
         (list #f #t #t #f #f #f #f #f #f #f #f #f #f #f #f #f)))

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
; produce the i of the first block of a free chunk of the requested size
(check-expect (find-chunk HBS-4 1) 1)
(check-expect (find-chunk HBS-4 2) 2)
(check-expect (find-chunk HBS-4 4) 4)
(check-expect (find-chunk HBS-4 8) -1)
(check-expect (find-chunk HBS-6 1) 6)
(check-expect (find-chunk HBS-7 4) -1)
(check-expect (find-chunk HBS-8 4) 0)
(check-expect (find-chunk HBS-8 1) 0)
(check-expect (find-chunk HBS-9 2) 14)


(define (find-chunk HBS s)
  (find-chunk-helper (reverse HBS) s 1))

(define (find-chunk-helper HBS s e)
  (cond
    [(empty? HBS) -1]
    [(and (>= e s) (> (first-true (first HBS)) -1)) (* e (first-true (first HBS)))]
    [else (find-chunk-helper (rest HBS) s (* e 2))]))


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
(check-expect (alloc-chunk HBS-1 2)
              (make-hbs-alloc 0 (list (list #f #t) (list #f #t #f #f) (list #f #f #f #f #f #f #f #f))))

(define (alloc-chunk HBS s)
  (local [(define i (find-chunk HBS s))]
    (cond
      [(= i -1) HBS]
      [else (make-hbs-alloc i (initialize-hbs (set-range #f (propagate HBS (last HBS)) i (+ -1 i s) 0)))])))

; propagate : HBS [List-of Boolean] -> [List-of [List-of Boolean]]
; Propagates the change in the HBS down the tree so all information for the HBS is encoded at the 
; bottom level
(define (propagate HBS lr)
  (cond
    [(empty? (rest HBS)) lr]
    [else (propagate (rest HBS) (encode-ranges (first HBS) lr 0 (/ (length lr) (length (first HBS)))) )]))

; encode-ranges : [List-of Boolean] [List-of Boolean] NonNegInteger NonNegInteger -> [List-of Boolean]
; Changes the bit as directed by the upper level of the HBS
(define (encode-ranges ur lr i factor)
  (cond
    [(empty? ur) lr]
    [(first ur)
     (encode-ranges (rest ur) 
                    (set-range #t lr (* i factor) (+ (* i factor) (- factor 1)) 0)
                    (+ 1 i)
                    factor)]
    [else (encode-ranges (rest ur) lr (+ 1 i) factor)]))

; set-range : Boolean [List-of Boolean] NonNegInteger NonNegInteger NonNegInteger -> [List-of Boolean]
; Sets the range of bits in the list to the given boolean value
(check-expect (set-range #t (list #f #f #f #f #f #f #f #f) 2 4 0) (list #f #f #t #t #t #f #f #f))
(check-expect (set-range #t (list #f #f #f #f #f #f #f #f) 0 7 0) (list #t #t #t #t #t #t #t #t))
(check-expect (set-range #f (list #t #t #t #t #t #t #t #t) 0 0 0) (list #f #t #t #t #t #t #t #t))
(check-expect (set-range #f (list #t #t #t #t #t #t #t #t) 7 7 0) (list #t #t #t #t #t #t #t #f))


(define (set-range b lob start end i)
  (cond
    [(empty? lob) '()]
    [(and (>= i start) (<= i end)) (cons b (set-range b (rest lob) start end (+ 1 i)))]
    [else (cons (first lob) (set-range b (rest lob) start end (+ 1 i)))]))


; free-chunk : HBS NonNegInteger NonNegInteger -> HBS
; consumes a HBS, a chunk size and starting block number, and produces a new HBS with the chunk freed
(check-expect (free-chunk (list (list #f #t) (list #f #t #f #f) (list #f #f #f #f #f #f #f #f)) 2 0)
              (list (list #t #t) (list #f #f #f #f) (list #f #f #f #f #f #f #f #f)))

(define (free-chunk HBS s i)
    (initialize-hbs (set-range #t (propagate HBS (last HBS)) i (+ -1 i s) 0)))