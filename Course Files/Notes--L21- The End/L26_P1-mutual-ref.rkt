;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname L26_P1-mutual-ref) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; MUTUALLY REFERENTIAL DATA

; ANNOUNCEMENTS
; HW questions?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/image)

; THE WEB

; Anatomy of a raw web page.

#|
<html>
  <head>
    ...
    <b>
      <ul>
        <li>blah blah blah</li>
      </ul>
    </b>
    ...
  </head>

  <body>
    3
    <ul>
      <li>First line.</li>
      <li>Line the second.</li>
      <li>3rd line</li>
    </ul>

    My first paragraph.

    <ol>
      <li>First line.</li>
      <li>Line the second.</li>
      <li>3rd line</li>
    </ol>

  </body>
</html>
|#

; The things in angle bracket "<...>" are called tags, and a web page is
; still (largely) built by putting together tags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's come up with a data definition for a very simple version of HTML.
; We want to support:
; - text
; - images
; - ordered lists (OL)
; - unordered lists (UL)

; We're going to call this data definition a Div, short for "division", a demarcated
; section of an HTML document.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct ol [content])
; A OL is a (make-ol [List-of Div])
 
(define-struct ul [content])
; A UL is a (make-ul [List-of Div])
 
; A Div is one of:
; - String
; - Image
; - OL
; - UL
; Interpretation: The structure of an HTML page,
; where OL is an ordered list and UL is an unordered list

; (NB: for simplicity's sake, we're not going to implement a BODY-type Div; also,
; we're inlining images.)
 
(define DIV-1 "Hi!")
(define DIV-2 "Hello!")
(define DIV-3 (star 40 "solid" "yellow"))
(define DIV-4 (make-ul (list DIV-1 DIV-2)))
(define DIV-5 (make-ol (list DIV-1 DIV-2 DIV-3 DIV-4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Create the templates for these data definitions.
;<DO NOW><SCROLL-RM>








(define (ol-temp ol)
  (... (lod-temp (ol-content ol)) ...))
 
(define (ul-temp ul)
  (... (lod-temp (ul-content ul)) ...))
 
(define (div-temp d)
  (...
   (cond [(string? d) (... d ...)]
         [(image? d) (... d ...)]
         [(ol? d) (... (ol-temp d) ...)]
         [(ul? d) (... (ul-temp d) ...)])))

; Following no required, but good to have
(define (lod-temp lod)
  (...
   (cond [(empty? lod) ...]
         [(cons? lod) (... (div-temp (first lod)) ...
                           (lod-temp (rest lod)) ...)])))

; Again, note the parallelism between references in the data definitions and
; recursion in the corresponding templates.
; Mutually referential data produces mutually recursive code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function num-divs that accepts a div and returns the total number
; of divs inside of it.

; num-divs : Div -> NatNum
; Counts the total number of Divs contained within this Div

#|
(check-expect (num-divs DIV-1) ???)
(check-expect (num-divs DIV-2) ???)
(check-expect (num-divs DIV-3) ???)
(check-expect (num-divs DIV-4) ???)
(check-expect (num-divs DIV-5) ???)
|#

; Design the code
 
(define (num-divs d)
  ; Start from template:
  ; <DO NOW><SCROLL-RM> helper: (num-divs/lod/v1 lod)
  #;
  (cond [(string? d) (... d ...)]
         [(image? d) (... d ...)]
         [(ol? d) (... (ol-temp d) ...)]
         [(ul? d) (... (ul-temp d) ...)])








  (cond [(string? d) 1]
        [(image? d) 1]
        [(ol? d) (add1 (num-divs/lod (ol-content d)))]
        [(ul? d) (add1 (num-divs/lod (ul-content d)))]))
 
; num-divs/lod : [List-of Div] -> NatNum
; Counts the total number of Divs in the list of Div's
 
(check-expect (num-divs/lod '()) 0)
(check-expect (num-divs/lod (list DIV-1 DIV-2 DIV-3)) 3)
(check-expect (num-divs/lod (list DIV-1 DIV-2 DIV-3 DIV-4)) 6)

(define (num-divs/lod/v1 lod)
  ; Start from template:
  ; <DO NOW><SCROLL> helper: (num-divs/lod lod)
  #;
  (cond [(empty? lod) ...]
         [(cons? lod) (... (div-temp (first lod)) ...
                           (lod-temp (rest lod)) ...)])







  
  (cond [(empty? lod) 0]
        [(cons? lod) (+ (num-divs (first lod))
                        (num-divs/lod/v1 (rest lod)))]))

; Exercise:
; Simplify via list abstractions.

(define (num-divs/lod lod)
  (foldr + 0 (map num-divs lod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function render-div that accepts a div and renders it like a
; web browser would: List should be indented, ULs should have bullets,
; OLs should have 1., etc..


; render-div : Div -> Image
; Renders this Div

(define TEXT-SIZE 12)
(define TEXT-COLOR "black")
(define BULLET (circle 2 "solid" "black"))
 
(define RENDER-1 (text DIV-1 TEXT-SIZE TEXT-COLOR))
(define RENDER-2 (text DIV-2 TEXT-SIZE TEXT-COLOR))
(define RENDER-3 DIV-3)
(define RENDER-4 (above/align "left"
                              (beside BULLET RENDER-1)
                              (beside BULLET RENDER-2)))
(define RENDER-5 (above/align "left"
                              (beside/align "top" (text "1. " TEXT-SIZE TEXT-COLOR) RENDER-1)
                              (beside/align "top" (text "2. " TEXT-SIZE TEXT-COLOR) RENDER-2)
                              (beside/align "top" (text "3. " TEXT-SIZE TEXT-COLOR) RENDER-3)
                              (beside/align "top" (text "4. " TEXT-SIZE TEXT-COLOR) RENDER-4)))
 
(check-expect (render-div DIV-1) RENDER-1)
(check-expect (render-div DIV-2) RENDER-2)
(check-expect (render-div DIV-3) RENDER-3)
(check-expect (render-div DIV-4) RENDER-4)
(check-expect (render-div DIV-5) RENDER-5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the code for render-div:

(define (render-div div)
  ; Start from template:
  ; <DO NOW><SCROLL-RM> helper: (num-divs/lod lod)
  #;
  (cond [(string? d) (... d ...)]
         [(image? d) (... d ...)]
         [(ol? d) (... (ol-temp d) ...)]
         [(ul? d) (... (ul-temp d) ...)])








  (cond [(string? div) (text div TEXT-SIZE TEXT-COLOR)]
        [(image? div) div]
        [(ol? div) (render-ol div)]
        [(ul? div) (render-ul div)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Implement the render-ul function.

; First, notice that the test cases could be slightly re-written:

(define RENDER-5-before (above/align "left"
                                     (beside/align "top" (text "1. " TEXT-SIZE TEXT-COLOR) RENDER-1)
                                     (beside/align "top" (text "2. " TEXT-SIZE TEXT-COLOR) RENDER-2)
                                     (beside/align "top" (text "3. " TEXT-SIZE TEXT-COLOR) RENDER-3)
                                     (beside/align "top" (text "4. " TEXT-SIZE TEXT-COLOR) RENDER-4)))
 
(define RENDER-5-after (above/align "left"
                        (beside/align "top" (text "1. " TEXT-SIZE TEXT-COLOR) RENDER-1)
                        (above/align "left"
                         (beside/align "top" (text "2. " TEXT-SIZE TEXT-COLOR) RENDER-2)
                         (above/align "left"
                          (beside/align "top" (text "3. " TEXT-SIZE TEXT-COLOR) RENDER-3)
                          (above/align "left"
                           (beside/align "top" (text "4. " TEXT-SIZE TEXT-COLOR) RENDER-4)
                           empty-image)))))

; In other words:
; (above DIV-1
;        DIV-2
;        DIV-3
;        ...
;        DIV-N)
;
; produces same visual results as:
; (above DIV-1
;        (above DIV-2
;               (above DIV-3
;                      ...
;                                  (above DIV-N
;                                         empty-image)...)))

; render-ul : UL -> Image
; Renders this UL

(check-expect (render-ul (make-ul '())) empty-image)
(check-expect (render-ul DIV-4) RENDER-4)
 
(define (render-ul ul)
  ;<DO NOW><SCROLL>







  
  (foldr (λ (new old) (above/align "left" new old))
         empty-image
         (map (λ (d) (beside BULLET (render-div d))) (ul-content ul))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Implement the render-ol function.

; render-ol : OL -> Image
; Renders this OL
(check-expect (render-ol (make-ol '())) empty-image)
(check-expect (render-ol DIV-5) RENDER-5)
(define (render-ol ol)
  (local [(define CONTENTS (ol-content ol))
          ; n->image : NatNum -> Image
          ; Produces the n'th element of the ordered list
          (define (n->image n)
            (beside/align "top"
                          (text (string-append (number->string (add1 n)) ". ")
                                TEXT-SIZE
                                TEXT-COLOR)
                          (render-div (nth CONTENTS n))))]
    (foldr (λ (new old) (above/align "left" new old))
           empty-image
           (build-list (length CONTENTS) n->image))))

; nth : [List-of Any] NatNum -> Any
; returns the nth item in a list, numbering from 0, or error if index is too large
(check-expect (nth (list "a" "b" "c") 0) "a")
(check-expect (nth (list "a" "b" "c") 1) "b")
(check-error (nth (list "a" "b" "c") 3))
(check-error (nth '() 0))
(define (nth l n)
  (cond [(empty? l) (error "bad index")]
        [(and (cons? l) (zero? n)) (first l)]
        [(and (cons? l) (positive? n)) (nth (rest l) (sub1 n))]))
