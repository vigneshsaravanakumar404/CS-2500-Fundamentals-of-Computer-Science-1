;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ======================================= Provided Data Designs ======================================
(define-struct hbs [bit left right])
; A HierarchicalBitmapSet (HBS) is one of:
; - #false
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
; - #false
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

(define HBS-2 (make-hbs #f END-TREE-F END-TREE-T))
;    0
;   / \
;  0   1

(define HBS-3 (make-hbs #t
                        (make-hbs #f (make-hbs #f END-TREE-F END-TREE-F) (make-hbs #f END-TREE-F END-TREE-F)) 
                        (make-hbs #f (make-hbs #f END-TREE-F END-TREE-F) (make-hbs #f END-TREE-F END-TREE-F))))
;                1
;              /   \
;            /       \
;          /           \
;        0               0
;      /   \           /   \
;    0       0       0       0
;   / \     / \     / \     / \
;  0   0   0   0   0   0   0   0

(define HBS-4 (make-hbs #f 
                        (make-hbs #f HBS-2 (make-hbs #f (make-hbs #t END-TREE-F END-TREE-F) (make-hbs #f END-TREE-F END-TREE-T))) 
                        HBS-3))
;                                0
;                            /       \
;                         /             \                   
;                      /                   \
;                   /                         \            
;                0                               1
;              /   \                           /   \
;            /       \                       /       \
;          /           \                   /           \
;        1               0               0               0
;      /   \           /   \           /   \           /   \
;    0       0       1       0       0       0       0       0
;   / \     / \     / \     / \     / \     / \     / \     / \
;  0   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0


(define HBS-5 (make-hbs #f
                        (make-hbs #f (make-hbs #f END-TREE-F END-TREE-F) (make-hbs #f END-TREE-F END-TREE-F)) 
                        (make-hbs #t (make-hbs #f END-TREE-F END-TREE-F) (make-hbs #f END-TREE-F END-TREE-F))))
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
                                                      END-TREE-F
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
    (local [
        (define (blocks-remaining-helper HBS weight)
            (cond
                [(and (hbs? HBS) (hbs-bit HBS)) weight]
                [(hbs? HBS) (+ (blocks-remaining-helper (hbs-left HBS) (/ weight 2))
                               (blocks-remaining-helper (hbs-right HBS) (/ weight 2)))]
                [(and #t HBS) weight]
                [else 0]))]
    (blocks-remaining-helper HBS (expt 2 (depth HBS)))))

; depth : HBS -> NonNegInteger
; to produce the depth of the HBS
(check-expect (depth END-TREE-T) 0)
(check-expect (depth HBS-2) 1)
(check-expect (depth HBS-3) 3)
(check-expect (depth HBS-4) 3)

(define (depth HBS)
  (local [(define (depth-helper HBS depth)
            (cond
              [(hbs? HBS) (depth-helper (hbs-left HBS) (+ depth 1))]
              [else depth]))]
    (+ -1 (depth-helper HBS 0))))


; Exercise 1b
; find-chunk : HBS NonNegInteger -> Integer
; to produce the first free chunk of given size in the hbs
(check-expect (find-chunk END-TREE-T 1) 0)
(check-expect (find-chunk HBS-2 1) 1)
(check-expect (find-chunk HBS-3 8) 0)
(check-expect (find-chunk HBS-3 4) 0)
(check-expect (find-chunk HBS-5 4) 4)
(check-expect (find-chunk END-TREE-F 1) -1)
(check-expect (find-chunk END-TREE-F 1) -1)
(check-expect (find-chunk HBS-3 4) 0)
(check-expect (find-chunk HBS-6 1) 6)
(check-expect (find-chunk HBS-7 4) -1)
(check-expect (find-chunk HBS-8 4) 0)
(check-expect (find-chunk HBS-9 2) 4)
;;; (check-expect (find-chunk HBS-4 8) 8)
;;; (check-expect (find-chunk HBS-4 16) -1)

(define (find-chunk HBS size)
    (local [(define result (find-chunk-helper HBS empty size 0 (expt 2 (depth HBS))))]
        (if (empty? result) -1 (hbs-chunk-block (last result)))))


(define (find-chunk-helper HBS pq size block-index weight)
    (local [(define half (/ weight 2))]
        (cond
            [(boolean? HBS) pq] ; Reached the end of HBS
            [(< weight size) pq] ; Went too far deep, not big enough
            [(hbs-bit HBS) (list (make-hbs-chunk block-index weight))] ; Best solution down this path
            [else (append ; Look for solutions
                    (find-chunk-helper (hbs-left HBS) pq size block-index half)
                    (find-chunk-helper (hbs-right HBS) pq size (+ block-index half) half))])))

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

; initialize-hbs : [List-of Boolean] -> HBS
; to produce a list of lists 
(find-chunk-helper HBS-9 empty 2 0 (expt 2 (depth HBS-9)))