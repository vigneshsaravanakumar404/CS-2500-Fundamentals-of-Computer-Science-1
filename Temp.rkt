; Figurative Data Struct:
; A Example is a Type ...
; Interpretation: Represents ...
(define EXAMPLE-1 Type)
(define EXAMPLE-2 Type)
(define EXAMPLE-3 Type)
(define (example-temp p)
  (... p ... ))

; Enumeration:
; A Enum is one of:
; - Type-1
; - Type-2
; Interpretation: A Enum represents ...
(define ENUM-1 "Type-1")
(define ENUM-2 "Type-2")
; enum-temp : Enum -> ?
(define (enum-temp m)
  (...
   (cond
     [(string=? m ENUM-1) ...]
     [(string=? m ENUM-2) ...])))

; Struct:
(define-struct my-struct [ ... f1 f2 f3 ... ])
; A My-struct is a (make-my-struct ... Type-1 Type-2 Type-3 ...)
; Interpretation: Represents ...
; - f1 is a Type1 ...
; - f2 is a Type2 ...
; - f3 is a Type3 ...
(define MY-STRUCT-1 (make-my-struct ... Type-1 Type-2 Type-3 ...))
(define MY-STRUCT-2 (make-my-struct ... Type-1 Type-2 Type-3 ...))
(define MY-STRUCT-3 (make-my-struct ... Type-1 Type-2 Type-3 ...))
(define (my-struct-temp f)
  (... (my-struct-f1 f) ...
       (my-struct-f2 f) ...
       (my-struct-f3 f) ...))

; Union:
; An Union is one of:
; - NaturalNumber
; - #false
; - My-struct
; Interpretation: A union abstracting a NaturalNumber a Boolean and a My-struct
 
(define UNION-1 #false)
(define UNION-2 17)
(define UNION-3 192388)
(define (union-temp u)
  (...
   (cond
     [(number? u) ...]
     [(boolean? u) ...]
     [(my-struct? u) (my-struct-temp ...)])))