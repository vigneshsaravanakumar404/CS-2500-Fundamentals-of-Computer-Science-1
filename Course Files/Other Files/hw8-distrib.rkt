;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw8-rubric-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; PROVIDED:

; A HierarchicalBitmapSet (HBS) is a [List-of [List-of Boolean]]
; (Note that this is just a shorthand synonym.)

; The following data definition must be used as the return type for several of your functions:
(define-struct hbs-alloc [block hbs])
; A HBSAllocResult is a (make-hbs-alloc Integer HBS)
; Represents the result of an allocation call, where:
; - block is the starting block number for the chunk that was allocated;
;   or -1 if no space available
; - hbs is the updated HBS after the allocation
(define HBSAR-1 (make-hbs-alloc 2
                                (list (list #f #t)
                                      (list #t #f #f #f))))
(define (hbsar-temp hbsar)
  (... (hbs-alloc-block hbsar) ...
       (hbs-temp (hbs-alloc-hbs hbsar) ...)))

; A ListOfBoolPairs (LoBP) is one of:
; - empty
; - (cons Boolean (cons Boolean ListOfBoolPairs))
; Represents an empty list, or the first pair of Booleans in an
; even-length list of Bools,
(define LOBP-0 '())
(define LOBP-1 (list #false #false))
(define LOBP-2 (append (list #true #true) LOBP-1))

(define (lobp-temp lobp)
  (...
   (cond [(empty? lobp) ...]
         [(cons? lobp) (... (first lobp) ...
                           (first (rest lobp)) ...
                           (lobp-temp (rest (rest lobp))) ...)])))
;; END--PROVIDED

; JYP: added a couple of extra examples
(define LOBP-3 (append (list #true #false) LOBP-2))
(define LOBP-4 (append (list #false #false) LOBP-3))
; So: LOBP-4 = (list #false #false #true #true #true #false #false #false)

; A HierarchicalBitmapSet (HBS) is a [List-of ListOfBoolPairs]]
; I am providing the template, which isn't necessary, but still helpful
(define (hbs-temp hbs)
  (cond [(empty? hbs) ...]
        [(cons? hbs) (... (lobp-temp (first hbs)) ...
                             (hbs-temp (rest hbs)) ...)]))

(define HBS-1 (list (list #f #t)
                    (list #t #f #f #f)
                    (list #f #f #t #f #f #f #f #f)
                    (list #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f)))
(define HBS-2 (list (list #f #f)
                    (list #t #f #f #f)
                    (list #f #f #f #f #f #f #f #f)
                    (list #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that we are aware there are more efficient ways to do each of the
;; following tasks. However, these are the simplest and most straightforward.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1:

;; Part A: Design the function blocks-remaining that consumes a HBS and produces
;; a count of the total number of free blocks (not chunks) available on the drive.

; blocks-remaining : HBS -> NatNum
; produces the total number of free blocks in an HBS, across all chunk sizes
(check-expect (blocks-remaining HBS-1) 15)
(check-expect (blocks-remaining HBS-2) 5)
(define (blocks-remaining hbs)
  (cond [(empty? hbs) 0]
        [(cons? hbs) (+ (* (count-trues (first hbs)) (top-chunksize hbs))
                        (blocks-remaining (rest hbs)))]))

  
;; Part B: Design the function find-chunk that consumes a HBS and a chunk size
;; (which will always be a power-of-2) and produce the index of the first block
;; of a free chunk of the requested size. Note that this function only returns a
;; Integer, since it does not modify the HBS.
;; Note that this should report the exact same chunk of blocks as what a true
;; allocation would return, but without actually making any changes to the HBS.

; find-chunk : HBS PosInt -> Nat
; returns the index of the first block of a free chunk of the requested size
; (Note: it might return a piece of a possibly bigger chunk if the requested level is empty.)
(check-expect (find-chunk HBS-2 1) 7) ; finds at chunksize=1
(check-expect (find-chunk HBS-2 2) 0) ; none free at chunksize=2, finds at 4
(check-expect (find-chunk HBS-2 8) -1) ; unfulfillable request
(check-expect (find-chunk HBS-2 16) -1) ; request beyond highest level

(define (find-chunk lolobp size)
  (local [(define chunksize (top-chunksize lolobp))]
    (cond [(< chunksize size) -1]
          [(= chunksize size) (chunk->blk chunksize (first-true (first lolobp)))]
          [(> chunksize size) (local [(define blk (find-chunk (rest lolobp) size))]
                                (if (negative? blk)
                                    (chunk->blk chunksize (first-true (first lolobp)))
                                    blk))]
)))

;; Part C: Design the function initialize-hbs that consumes a single bitmap of the
;; free blocks on a drive, and produces the corresponding HBS, set up according to
;; the representation design you chose (A or B). So, the signature for this should be:

; initialize-hbs : [List-of Boolean] -> HBS
; Builds a normal form multi-level HBS from a flat block allocation map
(check-expect (initialize-hbs (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t)) HBS-1)
(check-expect (initialize-hbs (list #t #t #t #t #f #f #f #t #f #f #f #f #f #f #f #f)) HBS-2)
(define (initialize-hbs lob)
  (local [(define numblocks (length lob))
          ; create-hbs : PosInt -> HBS
          ; Creates an HBS up to and including a top level w/given chunksize
          ; HBS is correct minimal form, but all upper bitmaps are empty
          ; (create-hbs 2 (list #t #t #f #t)) returns (list (list #f #f) (list #t #t #f #t))
          ; (create-hbs 2 (list #t #t #f #t #t #t #f #t)) returns
          ;               (list (list #f #f #f #f) (list #t #t #f #t #t #t #f #t))
          ; (create-hbs 4 (list #t #t #f #t #t #t #f #t)) returns
          ;               (list (list #f #f) (list #f #f #f #f) (list #t #t #f #t #t #t #f #t))
          (define (create-hbs chunksize)
            (cond [(= chunksize 1) (list lob)]
                  [else (cons (build-list (/ numblocks chunksize) negative?)
                        (create-hbs (/ chunksize 2)))]))]
    (normalize-hbs (create-hbs (/ numblocks 2)) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2:
;; Design the function alloc-chunk that consumes a HBS and a chunk size (which will
;; always be a power-of-2) and produces an HBSAllocResult including the index of the
;; first block of the free chunk of the requested size that was allocated, as well
;; as the resulting HSB modified to reflect the current allocation.
;; If the allocation fails because no appropriate chunk could be found, the block
;; number in the result structure should be -1, and the HBS should be returned unmodified.

; alloc-chunk : HBS PosInt -> HBSAllocResult
; finds at Level-0 (chunksize=1):
(check-expect (alloc-chunk HBS-2 1)
              (make-hbs-alloc 7 (list (list #f #f)
                                      (list #t #f #f #f)
                                      (list #f #f #f #f #f #f #f #f)
                                      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))))
; none free at Level-1 (chunksize=2), finds at Level-2 (chunksize=4), splits:
(check-expect (alloc-chunk HBS-2 2)
              (make-hbs-alloc 0 (list (list #f #f)
                                      (list #f #f #f #f)
                                      (list #f #t #f #f #f #f #f #f)
                                      (list #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f))))
(check-expect (alloc-chunk HBS-2 8) (make-hbs-alloc -1 HBS-2)) ; unfulfillable request
(check-expect (alloc-chunk HBS-2 16) (make-hbs-alloc -1 HBS-2)) ; request beyond highest level

(define (alloc-chunk hbs size)
  (local [(define blk (find-chunk hbs size))]
    (if (negative? blk)
        (make-hbs-alloc -1 hbs)
        (make-hbs-alloc blk (take-chunk hbs size blk)))))

; take-chunk : HBS PosInt Nat -> HBS
; Assumes the validity of the chunk to be allocated--it is an error if the
; chunk is not free at some level.
(define (take-chunk hbs size blk)
  (local [(define chunksize (top-chunksize hbs))
          (define chunk (blk->chunk chunksize blk))]
    (cond [(empty? hbs) (error "take-chunk: got to empty?")]
          [(cons? hbs) (cond [(< chunksize size) (error "take-chunk: level too deep")]
                             [(= chunksize size) (cons (set-false (first hbs) chunk) (rest hbs))]
                             [(> chunksize size)
                              (if (nth-is-true? (first hbs) chunk)
                                  (cons (set-false (first hbs) chunk)
                                        (take-chunk (cons (set-pair (first (rest hbs)) chunk)
                                                          (rest (rest hbs)))
                                                    size
                                                    blk))
                                  (cons (first hbs)
                                        (take-chunk (rest hbs) size blk)))])])))

; set-pair : LoBP -> LoBP
; Sets both members of the pair including bit n to #true; ignore request w/n >= length
(check-expect (set-pair LOBP-0 0) '())
(check-expect (set-pair LOBP-1 0) (list #t #t))
(check-expect (set-pair LOBP-4 2) (list #false #false #true #true #true #true #false #false))
(check-expect (set-pair LOBP-4 3) (list #false #false #true #true #true #true #false #false))
(check-expect (set-pair LOBP-4 4) LOBP-4)

(define (set-pair lobp n)
  (cond [(empty? lobp) lobp]
        [(cons? lobp) (if (>= n 2)
                          (cons (first lobp)
                                (cons (second lobp)
                                      (set-pair (rest (rest lobp)) (- n 2))))
                          (cons #t
                                (cons #t
                                      (rest (rest lobp)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3:
;; Design the function free-chunk that consumes a HBS, a chunk size, and the starting
;; block of the chunk, and produces an updated HBS that reflects the deallocation.
;; You can assume the chunk size is a power-of-2, appropriately aligned, and currently
;; not free.

; free-chunk : HBS PosInt Nat -> HBS
; Frees a chunk of specified size back into the HBS, making sure it retains normal form
(define TEST-FC-1 (list (list #f #f)
                        (list #t #f #f #f)
                        (list #f #f #f #f #f #f #f #f)
                        (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))
(define TEST-FC-2 (list (list #f #f)
                        (list #f #f #f #f)
                        (list #f #t #f #f #f #f #f #f)
                        (list #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f)))
(check-expect (free-chunk TEST-FC-1 1 7)
              HBS-2)
; none free at Level-1 (chunksize=2), finds at Level-2 (chunksize=4), splits:
(check-expect (free-chunk TEST-FC-2 2 0)
              HBS-2)
(define (free-chunk hbs size blk)
  (normalize-hbs (free-just-chunk hbs size blk) size))

; free-just-chunk : HBS PosInt Nat -> HBS
; Sets the bit corresponding to the chunk and level to #true
; Preserves correct minimal (but not normalized) form
(define (free-just-chunk hbs size blk)
  (local [(define chunksize (top-chunksize hbs))
          (define chunk (blk->chunk chunksize blk))]
    (cond [(empty? hbs) (error "free-chunk: level too deep")]
          [(cons? hbs) (if (= chunksize size)  ; at desired level
                           (cons (set-true (first hbs) chunk) (rest hbs))
                           (cons (first hbs) (free-just-chunk (rest hbs) size blk)))])))


(define (normalize-hbs hbs bottom-size)
  (local [(define chunksize (top-chunksize hbs))]
    (cond [(empty? hbs) (error "normalize-hbs: level too deep")]
          [(cons? hbs) (if (= chunksize bottom-size)
                           hbs
                           (normalize-this-level (first hbs)
                                                 (normalize-hbs (rest hbs) bottom-size)))])))

; normalize-this-level : LoBP [NEList-of LoBP] -> NEList-of LoBP]
; Adjusts level above given HBS by coallescing pairs at top-of-rest-of-HBS
; into the top bitmap, which is passed in separately
(check-expect (normalize-this-level (list #t #f) (list (list #f #f #t #f)))
              (list (list #t #f) (list #f #f #t #f)))
(check-expect (normalize-this-level (list #t #f) (list (list #f #f #t #t)))
              (list (list #t #t) (list #f #f #f #f)))
(define (normalize-this-level curr-level rest-of-levels)
  (cons (map or-func curr-level (find-all-pairs-of-true (first rest-of-levels)))
        (cons (clear-all-pairs (first rest-of-levels)) (rest rest-of-levels))))

(define (or-func x y)
  (or x y))


; find-all-pairs-of-true : LoBP -> LoBoolean
; For every pair of #true in the input list, produce a #true in the output list;
; else, corresponding #false; so, for input of length n, produces list of length n/2
(check-expect (find-all-pairs-of-true LOBP-0) '())
(check-expect (find-all-pairs-of-true LOBP-1) (list #f))
(check-expect (find-all-pairs-of-true LOBP-2) (list #t #f))
(check-expect (find-all-pairs-of-true LOBP-4) (list #f #f #t #f))

(define (find-all-pairs-of-true lobp)
  (cond [(empty? lobp) lobp]
        [(cons? lobp) (cons (and (first lobp) (first (rest lobp)))
                          (find-all-pairs-of-true (rest (rest lobp))))]))

; clear-all-pairs : LoBP -> LoBP
; clears (i.e., turns to #false) all pairs of #true
(check-expect (clear-all-pairs LOBP-0) '())
(check-expect (clear-all-pairs LOBP-1) (list #f #f))
(check-expect (clear-all-pairs LOBP-2) (list #f #f #f #f))
(check-expect (clear-all-pairs LOBP-4) (list #f #f #t #f #f #f #f #f))

(define (clear-all-pairs lop)
  (cond [(empty? lop) lop]
        [(cons? lop) (if (and (first lop) (second lop))
                         (cons #false
                               (cons #false
                                     (clear-all-pairs (rest (rest lop)))))
                         (cons (first lop)
                               (cons (second lop)
                                     (clear-all-pairs (rest (rest lop))))))]))

; blk->chunk : [NEList-of LoBP] Nat -> Nat
; Determines the index of the chunk at the top level of a HBS bitmap
; representing the specified block
(check-expect (blk->chunk 8 8) 1)
(check-expect (blk->chunk 2 8) 4)
(check-expect (blk->chunk 1 -1) -1)

(define (blk->chunk chunksize blk)
  (if (negative? blk)
      blk
      (quotient blk chunksize)))

; chunk->blk : PosInt Nat -> Nat
; Determines the index of the first block of the given chunk at the top level of a
; HBS bitmap representing the specified block
(check-expect (chunk->blk 8 1) 8)
(check-expect (chunk->blk 2 4) 8)
(check-expect (chunk->blk 1 -1) -1)
(define (chunk->blk chunksize c)
  (if (negative? c)
      c
      (* c chunksize)))

         
; top-chunksize : [NEList-of LoBP] -> PosInt
; Determines the block size of the top level of a Binary Buddy bitmap
(check-expect (top-chunksize HBS-1) 8)
(check-expect (top-chunksize (rest (rest HBS-1))) 2)
(check-expect (top-chunksize (list (list #t #f))) 1)

(define (top-chunksize lolobp)
  (expt 2 (sub1 (length lolobp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following is the canonical solution to HW6, which was provided to the students

;; Part A: Design the function count-trues
;; 6pt

; count-trues : [List-of Boolean] -> NatNum
; Produces the count of #true values in the list
; STUB: add check-expects
(define (count-trues lob)
  (cond [(empty? lob) 0]
        [(cons? lob) (if (first lob)
                           (add1 (count-trues (rest lob)))
                           (count-trues (rest lob)))]))

;; Part B: Design the function nth-is-true?
;; 6pt

; nth-is-true? : [List-of Boolean] NatNum -> Boolean
;; returns whether the value at position n (counting from 0) is #true.
; STUB: add check-expects
(define (nth-is-true? lob n)
  (cond [(empty? lob) #false]
        [(cons? lob) (if (zero? n)
                          (first lob)
                          (nth-is-true? (rest lob) (sub1 n)))]))

;; Part C: Design the function first-true
;; 7pt

;; GRADING: Avoid unnecessary recursion: they must stop recursing when they find first true.

; first-true : [List-of Boolean] -> NatNum
; returns the position (counting from 0) of the first #true, or -1 if none were found.
(define (first-true lob)
  (cond [(empty? lob) -1]
        [(cons? lob) (if (first lob)
                          0
                          (if (= -1 (first-true (rest lob)))
                              -1
                              (add1 (first-true (rest lob)))))]))

;; Part D: Design the function set-true
;; 6pt

;; GRADING: Avoid unnecessary recursion: they must stop recursing when they recurse to desired index.

; set-true : [List-of Boolean] NatNum -> [List-of Boolean]
; returns the list with the n-th item in the list converted from #false to #true
; Extra Credit: if the old value was not #false, throws an error
; STUB: add check-expects
(define (set-true lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #true (rest lob))
                          ; Alternative for Extra Credit:
                          ;(if (not (first lob))
                          ;    (cons #true (rest lob))
                          ;    (error "set-true: already #true"))
                          (cons (first lob) (set-true (rest lob) (sub1 n))))]))


;; Part E: Design the function set-false
;; 6pt

;; GRADING: Avoid unnecessary recursion: they must stop recursing when they recurse to desired index.

; set-false : [List-of Boolean] NatNum -> [List-of Boolean]
; returns the list with the n-th item in the list converted from #true to #false
; Extra Credit: if the old value was not #true, throws an error
; STUB: add check-expects
(define (set-false lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                          (cons #false (rest lob))
                          ; Alternative for Extra Credit:
                          ;(if (first lob)
                          ;    (cons #false (rest lob))
                          ;    (error "set-false: already #false"))
                          (cons (first lob) (set-false (rest lob) (sub1 n))))]))

;; Part F: Design the function draw-map
;; 7pt

;; GRADING: they can just make false a simple outline rectangle

(require 2htdp/image)

(define BOX-HEIGHT 16)

(define (draw-lomap lolobp)
  (local [(define (add-map-above lobp img)
            (above/align "left" (draw-map lobp)  img))]
    (foldr add-map-above empty-image lolobp)))

; draw-map : [List-of Boolean] -> Image
; displays the list as a series of contiguous rectangles of the requested width width:height ratio.
;; render the #true values as black filled boxes, and #false values as black-outlined white boxes.
; STUB: add check-expects
(define (draw-map lob n)
  (cond [(empty? lob) empty-image]
        [(cons? lob) (beside (if (first lob)
                                 (overlay (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "outline" "black")
                                          (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "solid" "black"))
                                 (overlay (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "outline" "black")
                                          (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "solid" "white")))
                             (draw-map (rest lob) n))]))
