;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; ====================================================================================
; End of Import Functions
; ====================================================================================
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
  (list (list #t #f)
        (list #f #f #t #f)
        (list #f #f #f #f #f #f #t #f)))

(define HBS-7
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
(check-expect (blocks-remaining (list (list #t))) 1)
(check-expect (blocks-remaining (list (list #f))) 0)

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
(check-expect (find-chunk HBS-5 4) -1)
(check-expect (find-chunk HBS-1 4) 0)
(check-expect (find-chunk HBS-1 1) 0)
(check-expect (find-chunk HBS-7 2) 14)
(check-expect (find-chunk (list (list #f)) 2) -1)


(define (find-chunk HBS s)
  (local [(define (find-chunk-helper HBS s e)
            (cond
              [(empty? HBS) -1]
              [(and (>= e s) (> (first-true (first HBS)) -1)) (* e (first-true (first HBS)))]
              [else (find-chunk-helper (rest HBS) s (* e 2))]))]
    (find-chunk-helper (reverse HBS) s 1)))



; Exercise 1c
; initialize-hbs : [List-of Boolean] -> HBS
; consumes a single bitmap of the free blocks on a drive, and produces the corresponding HBS 
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #t)) HBS-1)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #f #f)) HBS-2)
(check-expect (initialize-hbs (list #f #f #f #f #f #f #f #f)) HBS-5)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #f)) HBS-6)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #t)) HBS-1)
(check-expect (initialize-hbs (list #f #f)) (list (list #f #f)))
(check-expect (initialize-hbs (list #f)) (list #f))
(check-expect (initialize-hbs
               (list #t #t #t #t #t #t #t #t)) (list
                                                (list #t #t)
                                                (list #f #f #f #f)
                                                (list #f #f #f #f #f #f #f #f)))
(check-expect (initialize-hbs
               (list #f #f #f #f #f #f #f #f))  (list
                                                 (list #f #f)
                                                 (list #f #f #f #f) 
                                                 (list #f #f #f #f #f #f #f #f)))
(check-expect (initialize-hbs
               (list #t #t #t #t #t #t #t #f)) (list
                                                (list #t #f)
                                                (list #f #f #t #f)
                                                (list #f #f #f #f #f #f #t #f)))


(define (initialize-hbs lob)
  (local [(define (r lob o)
            (cond
              [(= (length lob) 2) (cons lob o)]
              [else (r (pairwise-and lob) (cons (coalesce lob) o))]))]
    (cond
      [(< (length lob) 2) lob]
      [else (r lob '())])))

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
              (make-hbs-alloc 0 (list (list #f #t)
                                      (list #f #t #f #f)
                                      (list #f #f #f #f #f #f #f #f))))

(check-expect (alloc-chunk HBS-2 2)
              (make-hbs-alloc 4
                              (list (list #t #f)
                                    (list #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f))))

(check-expect (alloc-chunk HBS-3 4)
              (make-hbs-alloc 0
                              (list (list #f #f)
                                    (list #f #f #t #f)
                                    (list #f #f #f #f #f #f #f #t))))


(check-expect (alloc-chunk (list (list #f #f)
                                 (list #f #f #f #f)
                                 (list #f #f #f #f #f #f #f #f)) 2) 
              (make-hbs-alloc -1
                              (list (list #f #f)
                                    (list #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f))))

(define (alloc-chunk HBS s)
  (local [(define i (find-chunk HBS s))]
    (if (= i -1)
        (make-hbs-alloc -1 HBS)
        (make-hbs-alloc i (abstraction HBS s i #f)))))

; Exercise 3
; free-chunk : HBS NonNegInteger NonNegInteger -> HBS
; consumes a HBS, a chunk size and starting block number, and produces a new HBS with the chunk freed
(check-expect (free-chunk (list (list #f #t) 
                                (list #f #f #t #t) 
                                (list #f #f #f #f #t #t #f #f)) 
                          2 4)
              (list (list #false #t)
                    (list #false #false #false #false)
                    (list #false #false #false #false #false #false #false #false)))

(check-expect (free-chunk (list (list #t #f) 
                                (list #t #t #f #f) 
                                (list #t #t #t #t #f #f #t #t)) 
                          2 4)
              (list (list #t #t)
                    (list #false #false #false #false)
                    (list #false #false #false #false #false #false #false #false))) 
(check-expect (free-chunk (list (list #t #f) 
                                (list #f #f #f #f) 
                                (list #f #f #f #f #t #f #f #f)) 
                          2 6)
              (list (list #t #false)
                    (list #false #false #false #t)
                    (list #false #false #false #false #t #false #false #false)))

(check-expect (free-chunk (list (list #f #f) 
                                (list #f #f #f #f) 
                                (list #f #f #f #f #f #f #f #f)) 
                          4 0)
              (list (list #t #false)
                    (list #false #false #false #false)
                    (list #false #false #false #false #false #false #false #false))) 


(define (free-chunk HBS s i)
  (abstraction HBS s i #t))

; abstraction : HBS NonNegInteger NonNegInteger Boolean -> HBS
; consumes a HBS, a chunk size, a starting block number, and a boolean value, and produces a new HBS
; where all the bits are represented in the lowest level of the HBS using propagation then the 
; HBS is reconstructed from the bottom up
(check-expect (abstraction HBS-1 2 (find-chunk HBS-1 2) #f) 
              (list (list #f #t)
                    (list #f #t #f #f)
                    (list #f #f #f #f #f #f #f #f)))

(check-expect (abstraction HBS-2 2 (find-chunk HBS-2 2) #f)
              (list (list #t #f)
                    (list #f #f #f #f)
                    (list #f #f #f #f #f #f #f #f)))

(check-expect (abstraction HBS-5
                           4
                           0
                           #t)
              (list (list #t #false)
                    (list #false #false #false #false)
                    (list #false #false #false #false #false #false #false #false)))

(check-expect (abstraction (list (list #t #false)
                                 (list #false #false #false #t)
                                 (list #false #false #false #false #t #false #false #false))
                           2
                           6
                           #t)
              (list (list #t #false)
                    (list #false #false #false #t)
                    (list #false #false #false #false #t #false #false #false)))

(define (abstraction HBS start i b)
  (local [
          ; encode-ranges : [List-of [List-of Boolean]] [List-of Boolean] NonNegInteger 
          ; NonNegInteger -> [List-of Boolean]
          ; Given a lob and the bottom row of the HBS, the function encodes that row into the bottom 
          ; row
          (define (encode-ranges row lower-row i ratio)
            (cond
              [(empty? row) lower-row]
              [(first row)
               (encode-ranges (rest row) 
                              (set-range #t lower-row (* i ratio) (+ (* i ratio) (- ratio 1)) 0)
                              (+ 1 i)
                              ratio)]
              [else (encode-ranges (rest row) lower-row (+ 1 i) ratio)]))
              
          ; propagate : [List-of [List-of Boolean]] [List-of Boolean] -> [List-of Boolean]
          ; Iterates through the HBS and calls each row to encode-ranges so that that row can be 
          ; represented in the bottom row. Also passes in the ratio of the current row to the bottom 
          ; row
          (define (propagate HBS lower-row)
            (if (empty? (rest HBS))
                lower-row
                (propagate (rest HBS)
                           (encode-ranges (first HBS) lower-row 0 (/ (length lower-row) (length (first HBS)))))))]
    (initialize-hbs (set-range b (propagate HBS (last HBS)) i (+ -1 i start) 0))))


; set-range : Boolean [List-of Boolean] NonNegInteger NonNegInteger NonNegInteger -> [List-of Boolean]
; Sets a range of values in a list to a given value specified by the boolean
(check-expect (set-range #t (list #f #f #f #f #f #f #f #f) 2 4 0) (list #f #f #t #t #t #f #f #f))
(check-expect (set-range #t (list #f #f #f #f #f #f #f #f) 0 7 0) (list #t #t #t #t #t #t #t #t))
(check-expect (set-range #f (list #t #t #t #t #t #t #t #t) 0 0 0) (list #f #t #t #t #t #t #t #t))
(check-expect (set-range #f (list #t #t #t #t #t #t #t #t) 7 7 0) (list #t #t #t #t #t #t #t #f))


(define (set-range b lob start end i)
  (cond
    [(empty? lob) '()]
    [(and (>= i start) (<= i end)) (cons b (set-range b (rest lob) start end (+ 1 i)))] 
    [else (cons (first lob) (set-range b (rest lob) start end (+ 1 i)))]))



; last : [List-of X] -> X
; Produces the last element of a list
(check-expect (last (list 1 2 3 4 5)) 5)
(check-expect (last (list 1 2 3 4)) 4)
(check-expect (last (list 1 2 3)) 3)

(define (last list)
  (first (reverse list)))


(alloc-chunk (list (list #t #f) (list #f #f #f #t)) 1)