;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ======================================= Provided Data Designs ======================================
(define-struct hbs [bit left right])
; A HierarchicalBitmapSet (HBS) is one of:
; - #f
; - (make-hbs Boolean HBS HBS)
; representing a bit at some level of a hierarchical tree of bits,
; or a sentinel value representing an empty tree/subtree
(define (hbs-temp hbs)
  (...
   (cond [(boolean? hbs) ...]
         [(hbs? hbs) (... (hbs-bit hbs) ...
                          (hbs-temp (hbs-left hbs)) ...
                          (hbs-temp (hbs-right hbs)) ...)])))


(define-struct hbs-alloc [block hbs])
; A HBSAllocResult is a (make-hbs-alloc Integer HBS)
; Represents the result of an allocation call, where:
; - block is the starting block number for the chunk that was allocated;
;   or -1 if no space available
; - hbs is the updated HBS after the allocation

(define HBSAR-1 (make-hbs-alloc 1
                                (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #t #f #f)
                                                    (make-hbs #f #f #f))
                                          (make-hbs #t
                                                    (make-hbs #f #f #f)
                                                    (make-hbs #f #f #f)))))

(define-struct hbs-chunk [block size])
; A HBSChunk is one of:
; - (make-hbs-chunk NatNum -1)
; - (make-hbs-chunk NatNum PosInt)
; Represents the result of a chunk query, where:
; - block is the starting block number for the chunk that was allocated;
; - size is the size chunk that is being taken or split
; or a sentinal value to indicate there was no eligible chunk
; ===================================== End Provided Functions =======================================

(define END-TREE-F (make-hbs #f #f #f))
;    0

(define END-TREE-T (make-hbs #t #f #f))
;    1

(define HBS-0 (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))))

(define HBS-2 (make-hbs #f END-TREE-F END-TREE-T))
;    0
;   / \
;  0   1

(define HBS-3 (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f END-TREE-F END-TREE-F)
                                  (make-hbs #f END-TREE-F END-TREE-F)) 
                        (make-hbs #f
                                  (make-hbs #f END-TREE-F END-TREE-F)
                                  (make-hbs #f END-TREE-F END-TREE-F))))
;                1
;              /   \
;            /       \
;          /           \
;        0               0
;      /   \           /   \
;    0       0       0       0
;   / \     / \     / \     / \
;  0   0   0   0   0   0   0   0


(define HBS-5 (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #f END-TREE-F END-TREE-F)
                                  (make-hbs #f END-TREE-F END-TREE-F)) 
                        (make-hbs #t
                                  (make-hbs #f END-TREE-F END-TREE-F)
                                  (make-hbs #f END-TREE-F END-TREE-F))))
;                0
;              /   \
;            /       \
;          /           \
;        0               1
;      /   \           /   \
;    0       0       0       0
;   / \     / \     / \     / \
;  0   0   0   0   0   0   0   0


(define HBS-6 (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #t #f #f)
                                            (make-hbs #f #f #f)))))

(define HBS-7 (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(define HBS-8 (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(define HBS-9 (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #f
                                            (make-hbs #t
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #t
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))))
;                                0
;                            /       \
;                         /             \                   
;                      /                   \
;                   /                         \            
;                0                               0
;              /   \                           /   \
;            /       \                       /       \
;          /           \                   /           \
;        1               0               0               1
;      /   \           /   \           /   \           /   \
;    0       0       1       0       0       1       0       0
;   / \     / \     / \     / \     / \     / \     / \     / \
;  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0

(define HBS-10 (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))

(define HBS-11 (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #t #f #f)
                                   (make-hbs #f #f #f))))
(define HBS-12 (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #t #f #f))))

; Exercise 1a
; blocks-remaining : HBS -> NonNegInteger
; to produce the number of blocks remaining in the HBS
(check-expect (blocks-remaining END-TREE-T) 1)
(check-expect (blocks-remaining HBS-2) 1)
(check-expect (blocks-remaining HBS-3) 8)
(check-expect (blocks-remaining HBS-6) 7)
(check-expect (blocks-remaining HBS-7) 0)
(check-expect (blocks-remaining HBS-8) 8)

(define (blocks-remaining HBS)
  (local [; blocks-remaining-helper : HBS NonNegInteger -> NonNegInteger
          ; to produce the number of blocks remaining in the HBS
          ; (blocks-remaining-helper HBS 2) should return 1
          ; (blocks-remaining-helper HBS 3) should return 8
          ; (blocks-remaining-helper HBS 4) should return 7
          (define (blocks-remaining-helper HBS weight)
            (cond
              [(and (hbs? HBS) (hbs-bit HBS)) weight]
              [(hbs? HBS) (+ (blocks-remaining-helper (hbs-left HBS) (/ weight 2))
                             (blocks-remaining-helper (hbs-right HBS) (/ weight 2)))]
              [else 0]))]
    (blocks-remaining-helper HBS (depth HBS))))

; depth : HBS -> NonNegInteger
; to produce the depth of the HBS
(check-expect (depth END-TREE-T) 1)
(check-expect (depth HBS-2) 2)
(check-expect (depth HBS-3) 8)
(check-expect (depth HBS-5) 8)

(define (depth HBS)
  (local [; depth-helper : HBS NonNegInteger -> NonNegInteger
          ; to produce the depth of the HBS
          ; (depth-helper HBS-3 0) should return 3
          ; (depth-helper HBS-3 1) should return 2
          ; (depth-helper HBS-3 2) should return 1
          (define (depth-helper HBS depth)
            (cond
              [(hbs? HBS) (depth-helper (hbs-left HBS) (+ depth 1))]
              [else depth]))]
    (expt 2 (+ -1 (depth-helper HBS 0)))))


; Exercise 1b
; find-chunk : HBS NonNegInteger -> Integer
; to produce the first free chunk of given size in the hbs
(check-expect (find-chunk HBS-2 1) 1)
(check-expect (find-chunk HBS-3 8) 0)
(check-expect (find-chunk HBS-3 4) 0)
(check-expect (find-chunk HBS-5 4) 4)
(check-expect (find-chunk HBS-6 1) 6)
(check-expect (find-chunk HBS-7 4) -1)
(check-expect (find-chunk HBS-8 4) 0)
(check-expect (find-chunk HBS-9 2) 4)
(check-expect (find-chunk END-TREE-T 1) 0)
(check-expect (find-chunk END-TREE-F 1) -1)
(check-expect (find-chunk END-TREE-F 1) -1)

(define (find-chunk HBS size)
  (hbs-chunk-block (find-chunk-helper HBS (make-hbs-chunk -1 size) size 0 (depth HBS))))

; find-chunk-helper : hbs hbs-chunk NonNegInt NonNegInt NonNegInt -> hbs-chunk
; finds a chunk of the requested size recursively storing the best solution so far
(check-expect (find-chunk-helper HBS-2 (make-hbs-chunk -1 1) 1 0 (depth HBS-2)) (make-hbs-chunk 1 1))
(check-expect (find-chunk-helper HBS-3 (make-hbs-chunk -1 8) 8 0 (depth HBS-3)) (make-hbs-chunk 0 8))
(check-expect (find-chunk-helper HBS-3 (make-hbs-chunk -1 4) 4 0 (depth HBS-3)) (make-hbs-chunk 0 8))
(check-expect (find-chunk-helper HBS-5 (make-hbs-chunk -1 4) 4 0 (depth HBS-5)) (make-hbs-chunk 4 4))
(check-expect (find-chunk-helper HBS-6 (make-hbs-chunk -1 1) 1 0 (depth HBS-6)) (make-hbs-chunk 6 1))
(check-expect (find-chunk-helper HBS-7 (make-hbs-chunk -1 4) 4 0 (depth HBS-7)) (make-hbs-chunk -1 4))
(check-expect (find-chunk-helper HBS-8 (make-hbs-chunk -1 4) 4 0 (depth HBS-8)) (make-hbs-chunk 0 8))
(check-expect (find-chunk-helper HBS-9 (make-hbs-chunk -1 2) 2 0 (depth HBS-9)) (make-hbs-chunk 4 2))

(define (find-chunk-helper HBS best size block-index weight)
  (local [(define half (/ weight 2))
          (define DNE (make-hbs-chunk -1 size))]
    (cond
      [(boolean? HBS) DNE] ; Reached the end of HBS
      [(< weight size) DNE] ; Went too deep, not big enough
      [(hbs-bit HBS) (make-hbs-chunk block-index weight)] ; Best solution down this path
      [else (better-chunk ; Look for best solution
             (find-chunk-helper (hbs-left HBS) best size block-index half)
             (find-chunk-helper (hbs-right HBS) best size (+ block-index half) half))])))

; better-chunk : hbs-chunk hbs-chunk -> hbs-chunk
; Compares which chunk is better
(check-expect (better-chunk (make-hbs-chunk -1 2) (make-hbs-chunk 2 2)) (make-hbs-chunk 2 2))
(check-expect (better-chunk (make-hbs-chunk 2 2) (make-hbs-chunk 2 2)) (make-hbs-chunk 2 2))
(check-expect (better-chunk (make-hbs-chunk 0 2) (make-hbs-chunk 2 2)) (make-hbs-chunk 0 2))
(check-expect (better-chunk (make-hbs-chunk 0 4) (make-hbs-chunk 2 2)) (make-hbs-chunk 2 2))

(define (better-chunk c1 c2)
  (local ((define a1 (hbs-chunk-size c1))
          (define a2 (hbs-chunk-size c2))
          (define b1 (hbs-chunk-block c1))
          (define b2 (hbs-chunk-block c2)))
    (cond [(= b1 -1) c2]
          [(= b2 -1) c1]
          [(< a1 a2) c1]
          [(> a1 a2) c2]
          [(< b1 b2) c1]
          [else c2])))


; last : [List-of Any] -> Any
; produces the last element of a list or returns empty if empty
(check-expect (last empty) empty)
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list "1" "2" "3")) "3")
(check-expect (last (list (list "1" 1) (list "2" 2) (list "3" 3))) (list "3" 3))

(define
  (last lst)
  (cond
    [(empty? lst) lst]
    [else (first (reverse lst))]))

; Exercise 1c
; initialize-hbs : [List-of Boolean] -> HBS
; to produce a HBS from a list of booleans
(check-expect (initialize-hbs (list #f)) END-TREE-F)
(check-expect (initialize-hbs (list #t)) END-TREE-T)
(check-expect (initialize-hbs (list #f #t)) HBS-2)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #t)) HBS-3)
(check-expect (initialize-hbs (list #f #f #f #f #t #t #t #t)) HBS-5)
(check-expect (initialize-hbs (list #t #t #t #t #t #t #t #f)) HBS-6)

(define (initialize-hbs LoB)
  (local [
          ; initialize-hbs-helper : [List-of Boolean] NonNegInteger NonNegInteger -> HBS
          ; to produce a HBS from a list of booleans
          ; (initialize-hbs-helper (list #t #t #t #t #t #t #t #t) 0 8) should return HBS-3
          ; (initialize-hbs-helper (list #t #t #t #t #t #t #t #t) 0 4) should return (hbs-left HBS-3)
          ; (initialize-hbs-helper (list #t #t #t #t #t #t #t #t) 4 4) should return (hbs-right HBS-3)
          (define (initialize-hbs-helper LoB index weight)
            (local
              [(define HALF (/ weight 2))
               (define VAL (and-range LoB index (+ index weight -1)))
               (define NEW-LIST (set-range VAL LoB index (+ index weight) 0))]
              (cond
                [(= weight 1) (if VAL END-TREE-T END-TREE-F)]
                [else (make-hbs VAL
                                (initialize-hbs-helper NEW-LIST index HALF)
                                (initialize-hbs-helper NEW-LIST (+ index HALF) HALF))])))]
    (initialize-hbs-helper LoB 0 (length LoB))))



; and-range : [List-of Boolean] NonNegInteger NonNegInteger -> Boolean
; to produce true if all elements in the list from start to end are true
(check-expect (and-range (list #f #t #t #f) 0 3) #f)
(check-expect (and-range (list #f #t #t #f) 0 2) #f)
(check-expect (and-range (list #f #t #t #f) 1 2) #t)
(check-expect (and-range (list #f #t #t #f) 1 3) #f)
(check-expect (and-range (list #f #t #t #f) 1 10) #f)
(check-expect (and-range (list #f #t #t #f) 0 0) #f)
(check-expect (and-range (list #t #f #f #f) 0 0) #t)

(define (and-range LoB start end)
  (local [
          ; helper : NonNegInteger -> Boolean
          ; to produce true if all elements in the list from start to end are true
          ; for LoB = (list #f #t #t #f) start = 0 end = 3 (helper 0) should return #f 
          ; for LoB = (list #f #t #t #f) start = 1 end = 2 (helper 1) should return #t
          ; for LoB = (list #f #t #t #f) start = 1 end = 10 (helper 1) should return #f
          ; for LoB = (list #f #t #t #f) start = 0 end = 0 (helper 0) should return #f
          (define (helper idx)
            (cond
              ((> idx end) #t)
              ((not (list-ref LoB idx)) #f)
              (else (helper (add1 idx)))))]
    (if (or (< start 0) (> end (sub1 (length LoB))) (> start end))
        #f
        (helper start))))

; set-range : Boolean [List-of Boolean] NonNegInteger NonNegInteger NonNegInteger -> [List-of Boolean]
; Sets a range of values in a list to a false if b is true
(check-expect (set-range #t (list #f #f #f #f #f #f #f #f) 2 4 0) (list #f #f #f #f #f #f #f #f))
(check-expect (set-range #t (list #f #f #f #f #f #f #f #f) 0 7 0) (list #f #f #f #f #f #f #f #f))
(check-expect (set-range #f (list #t #t #t #t #t #t #t #t) 0 0 0) (list #t #t #t #t #t #t #t #t))
(check-expect (set-range #f (list #t #t #t #t #t #t #t #t) 7 7 0) (list #t #t #t #t #t #t #t #t))

(define (set-range b lob start end i)
  (cond
    [(empty? lob) '()]
    [(> i end) lob]
    [(< i start) (cons (first lob) (set-range b (rest lob) start end (+ 1 i)))]
    [b (cons #f (set-range b (rest lob) start end (+ 1 i)))]
    [else (cons (first lob) (set-range b (rest lob) start end (+ 1 i)))]))


; Exercise 2
; alloc-chunk : hbs NonNegInt -> HBSAllocResult
; returns an HBSAllocResult with modified hbs allocting a chunk
(check-expect (alloc-chunk END-TREE-F 1) (make-hbs-alloc -1 END-TREE-F))
(check-expect (alloc-chunk HBS-0 1) (make-hbs-alloc -1 HBS-0))
(check-expect (alloc-chunk HBS-3 8)
              (make-hbs-alloc 0 (make-hbs #f (hbs-left HBS-3) (hbs-right HBS-3))))
(check-expect (alloc-chunk HBS-3 8)
              (make-hbs-alloc 0 (make-hbs #f (hbs-left HBS-3) (hbs-right HBS-3))))
(check-expect (alloc-chunk HBS-5 4)
              (make-hbs-alloc 4 (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f)))
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))))))
(check-expect (alloc-chunk HBS-5 2)
              (make-hbs-alloc 4 (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f)))
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #t
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))))))
(check-expect (alloc-chunk HBS-5 1)
              (make-hbs-alloc 4 (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f)))
                                          (make-hbs #f (make-hbs #f (make-hbs #f #f #f)
                                                                 (make-hbs #t #f #f))
                                                    (make-hbs #t (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))))))
(check-expect (alloc-chunk HBS-9 8) (make-hbs-alloc -1 HBS-9))
(check-expect (alloc-chunk HBS-10 32) (make-hbs-alloc -1 HBS-10))
(check-expect (alloc-chunk HBS-10 1)
              (make-hbs-alloc 6 (make-hbs #f
                                          (make-hbs #t
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f)))
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #t #f #f))))))
(check-expect (alloc-chunk HBS-10 2)
              (make-hbs-alloc 6 (make-hbs #f
                                          (make-hbs #t
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f)))
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))))))
(check-expect (alloc-chunk HBS-10 4)
              (make-hbs-alloc 0 (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f)))
                                          (make-hbs #f
                                                    (make-hbs #f
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))
                                                    (make-hbs #t
                                                              (make-hbs #f #f #f)
                                                              (make-hbs #f #f #f))))))

(define (alloc-chunk hbs n)
  (local [(define i (find-chunk hbs n))]
    (make-hbs-alloc i (if (= i -1) hbs (alloc-chunk-helper hbs i 0 n (depth hbs))))))

; alloc-chunk-helper : hbs NonNegInt NonNegInt NonNegInt NonNegInt -> HBS
; to produce a HBSAllocResult with modified hbs allocating a chunk
(check-expect (alloc-chunk-helper HBS-9 (find-chunk HBS-8 4) 0 4 (depth HBS-8))
              (make-hbs #f (make-hbs #f
                                     (make-hbs #t
                                               (make-hbs #f
                                                         (make-hbs #f #f #f)
                                                         (make-hbs #f #f #f))
                                               (make-hbs #f
                                                         (make-hbs #f #f #f)
                                                         (make-hbs #f #f #f)))
                                     (make-hbs #f
                                               (make-hbs #t
                                                         (make-hbs #f #f #f)
                                                         (make-hbs #f #f #f))
                                               (make-hbs #f (make-hbs #f #f #f)
                                                         (make-hbs #f #f #f))))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #t (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))))
(check-expect (alloc-chunk-helper HBS-9 (find-chunk HBS-9 1) 0 1 (depth HBS-9))
              (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #f
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #t #f #f))
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #t
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))))

(check-expect (alloc-chunk-helper HBS-9 (find-chunk HBS-9 2) 0 2 (depth HBS-9))
              (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #f
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #t
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f)))
                                  (make-hbs #t
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))
                                            (make-hbs #f
                                                      (make-hbs #f #f #f)
                                                      (make-hbs #f #f #f))))))

(define (alloc-chunk-helper hbs ti index tw weight)
  (local
    [(define new-hbs (if (hbs-bit hbs) (break-up-chunk hbs #f) hbs))
     (define half (/ weight 2))]  
    (cond
      [(and (= ti index) (<= weight tw))  (make-hbs #f (hbs-left hbs) (hbs-right hbs))]
      [(< ti (+ index half)) (make-hbs (hbs-bit new-hbs)
                                       (alloc-chunk-helper (hbs-left new-hbs) ti index tw half)
                                       (hbs-right new-hbs))]
      [else (make-hbs (hbs-bit new-hbs)
                      (hbs-left new-hbs)
                      (alloc-chunk-helper (hbs-right new-hbs) ti (+ index half) tw half))])))
       
; break-up-chunk : hbs boolean -> hbs
; to produce a HBSAllocResult with modified hbs allocating a chunk
(check-expect (break-up-chunk HBS-3 #f)
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (break-up-chunk HBS-3 #t)
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))
(check-expect (break-up-chunk END-TREE-F #f) (make-hbs #f #f #f))

(define (break-up-chunk hbs b)
  (cond
    [(boolean? hbs) hbs]
    [b (make-hbs b (hbs-left hbs) (hbs-right hbs))]
    [else (make-hbs b (break-up-chunk (hbs-left hbs) #t) (break-up-chunk (hbs-right hbs) #t))]))


; Exercise 3
; free-chunk : hbs NonNegInt NonNegInt -> hbs
; produces an updated HBS that reflects deallocation
(check-expect (free-chunk HBS-5 4 0)
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f END-TREE-F END-TREE-F)
                                  (make-hbs #f END-TREE-F END-TREE-F)) 
                        (make-hbs #f
                                  (make-hbs #f END-TREE-F END-TREE-F)
                                  (make-hbs #f END-TREE-F END-TREE-F))))
(check-expect (free-chunk HBS-11 1 3) (make-hbs #t
                                                (make-hbs #f
                                                          (make-hbs #f #f #f)
                                                          (make-hbs #f #f #f))
                                                (make-hbs #f
                                                          (make-hbs #f #f #f)
                                                          (make-hbs #f #f #f))))
(check-expect (free-chunk HBS-12 1 2) (make-hbs #f
                                                (make-hbs #f
                                                          (make-hbs #f #f #f)
                                                          (make-hbs #f #f #f))
                                                (make-hbs #t
                                                          (make-hbs #f #f #f)
                                                          (make-hbs #f #f #f))))
(check-expect (free-chunk HBS-12 1 1) (make-hbs #f
                                                (make-hbs #f
                                                          (make-hbs #f #f #f)
                                                          (make-hbs #t #f #f))
                                                (make-hbs #f
                                                          (make-hbs #f #f #f)
                                                          (make-hbs #t #f #f))))

(define (free-chunk hbs tw ti)
  (local [
          ; free-chunk-helper : hbs NonNegInt NonNegInt NonNegInt NonNegInt -> hbs
          ; to produce an updated HBS that reflects deallocation
          ; (free-chunk-helper HBS-5 4 0 4) should return (free-chunk HBS-5 4 0)
          ; (free-chunk-helper HBS-5 4 0 8) should return (free-chunk HBS-5 4 0)
          ; (free-chunk-helper HBS-5 4 0 2) should return (free-chunk HBS-5 4 0)
          (define (free-chunk-helper hbs tw ti w i)
            (local [(define half (/ w 2))
                    (define r (+ i half))]
              (cond
                [(= tw w) (make-hbs #t (hbs-left hbs) (hbs-right hbs))]
                [(< ti r) (combine (free-chunk-helper (hbs-left hbs) tw ti half i) (hbs-right hbs))]
                [else (combine (hbs-left hbs) (free-chunk-helper (hbs-right hbs) tw ti half r))])))]
    (free-chunk-helper hbs tw ti (depth hbs) 0)))

; combine : hbs hbs -> hbs
; to combine two hbs into one, if both hbs bits are #t then set them to #f and make parent #t
; otherwise make parent #f
(check-expect (combine (make-hbs #t #f #f) (make-hbs #t #f #f)) 
              (make-hbs #t (make-hbs #f #f #f) (make-hbs #f #f #f)))
(check-expect (combine (make-hbs #f #f #f) (make-hbs #t #f #f)) 
              (make-hbs #f (make-hbs #f #f #f) (make-hbs #t #f #f)))
(check-expect (combine END-TREE-T END-TREE-T) (make-hbs #t (make-hbs #f #f #f) (make-hbs #f #f #f)))

(define (combine l r)
  (if (and (hbs? l) (hbs? r) (hbs-bit l) (hbs-bit r))
      (make-hbs #t (make-hbs #f (hbs-left l) (hbs-right l)) (make-hbs #f (hbs-left r) (hbs-right r)))
      (make-hbs #f l r)))
