;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname P1-graph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; GRAPHS
;
; From an earlier class, where we wanted to define a website consisting of a
; tree of webpages

(define-struct page [title links])
 
; A WebPage is a (make-page String [List-of WebPage])
; Interpretation: A web page's title and links to other pages.

(define PAGE-0 (make-page "Khoury" '()))
(define PAGE-1 (make-page "NEU" (list PAGE-0)))
(define PAGE-2 (make-page "Boston" (list PAGE-0 PAGE-1)))

; Our data definitions were sufficient for the website we were trying to build then.
; But what if the Northeastern webmaster wanted to expand our site to help our
; first-years get orientated (love that neologism!) by adding a link from NEU back
; to the Boston page?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise	Modify our data examples so that NEU links to Boston too.
;<DO NOW> Modify example above in-place

; What happens when we try to run this?
;<DO NOW>







; Maybe this is because we're trying to create the examples in the wrong order.
; Can we resolve it by reordering our examples?
;<DO NOW>






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's take a cue from what real webpages do; it's not for the same reason,
; but still an idea that might be useful for addressing our problem:

; In HTML, outgoing links are represented as strings, called "URL"s, that don't
; represent a concrete page, but rather where to find the desired page.

; Our define-struct stays the same, but what the links field represents changes:


; A WebPage is a (make-page String [List-of String])
; Interpretation: a web page's title and links to other pages.

(define PAGE-0 (make-page "Khoury" (list "NEU")))
(define PAGE-1 (make-page "NEU" (list "Khoury" "Boston")))
(define PAGE-2 (make-page "Boston" (list "NEU" "Shillman Hall")))
(define PAGE-3 (make-page "Shillman Hall" (list "NEU")))
(define PAGE-4 (make-page "New Orleans" (list "Mardi Gras")))

(define (wp-temp wp)
  (... (page-title wp) ...
       (los-temp (page-links wp)) ...))

; However, this creates a new problem. We see the NEU page has a link to a page
; with the title "Khoury", but how do we now get from that String to the actual
; Khoury page itself: PAGE-0??
;<BRAINSTORM>








; We need a central repository of actual webpages, which we can then search
; for a page with a given title.

; A Wiki is a [List-of WebPage]
; Interpretation: A list of pages in a wiki
 
(define WIKI-1 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3 PAGE-4))
 
(define (wiki-temp w)
  (...
   (cond [(empty? w) ...]
         [(cons? w) (... (wp-temp (first w)) ...
                         (wiki-temp (rest w)) ...)])))

; Note: we now have to worry about the possibility that a linked-to page
; might not exist! (Can you say "404 ERROR"? I knew you could!)
; Why wasn't this a problem with our prvious design?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRAPHS
;
; Our original website had a structure that we in computer science call
; a tree. What we are trying to create now is what is called a graph.
; All trees are graphs, but not all graphs are trees. Specifically,
; trees do not have any cycles, and that is what's causing our difficulties.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function num-pages that accepts a Wiki and returns the number
; of pages mentioned anywhere in the Wiki.

; Do the first three steps of the design recipe.

; num-pages : Wiki -> Nat
; Returns the number of pages in a wiki
 
(check-expect (num-pages (list PAGE-0)) 2)
(check-expect (num-pages WIKI-1) 6)


; Now implement the function.

; Start by making a wish: if we had a function that simply returns all pages –
; we can just take the length of the result:
#;
(define (num-pages w)
  (local [(define (get-all-pages w)
            ...)
          ...])
  (length (get-all-pages w)))


; Now implement that helper.

(define (num-pages w)
  (local [; get-all-pages : Wiki -> [List-of String]
          ; returns all of the pages
          (define (get-all-pages w)
            ; Start from template:
            ; <DO NOW><SCROLL-RM>
            #;
            (cond [(empty? w) ...]
                  [(cons? w) (... (wp-temp (first w)) ...
                                  (wiki-temp (rest w)) ...)])








            
            (cond [(empty? w) '()]
                  [(cons? w) (append (list (page-title (first w)))
                                     (page-links (first w))
                                     (get-all-pages (rest w)))])
            )]
    (length (get-all-pages w))))

; Try running the program.
;<DO NOW>






; Why is get-all-pages returning 12?



; Duplicates. We should only add the page to the list if it doesn’t already exist in it. Thus we need another helper function (or two).

; num-pages : Wiki -> Nat
; Returns the number of pages in a wiki
 
(check-expect (num-pages (list PAGE-0)) 2)
(check-expect (num-pages WIKI-1) 6)

#;
(define (num-pages wiki)
  (local [; get-all-pages : Wiki -> [List-of String]
          ; Gets all the page names from the wiki
          (define (get-all-pages w)
            ; Starting w/original version:
            #;
            (cond [(empty? w) '()]
                  [(cons? w) (append (list (page-title (first w)))
                                     (page-links (first w))
                                     (get-all-pages (rest w)))])
            ;<DO NOW><SCROLL> helper: (merge LoS LoS)







            
            (cond [(empty? w) '()]
                  [(cons? w) (merge (cons (page-title (first w))
                                          (page-links (first w)))
                                    (get-all-pages (rest w)))]))
 
          ; merge : [List-of String] [List-of String] -> [List-of String]
          ; adds all those strings of the first list to the second
          ; that don't already exist in that list
          (define (merge l1 l2)
            (cond [(empty? l1) l2]
                  [(cons? l1)
                   (local [(define MERGED (merge (rest l1) l2))]
                     (if (s-in-los? (first l1) MERGED)
                         MERGED
                         (cons (first l1) MERGED)))]))]
    (length (get-all-pages wiki))))

; s-in-los? : String [List-of String] -> Boolean
; is the string in the list?

(check-expect (s-in-los? "a" '()) #false)
(check-expect (s-in-los? "a" (list "a" "b" "c")) #true)
(check-expect (s-in-los? "a" (list "c" "b" "a")) #true)
(check-expect (s-in-los? "a" (list "c" "b" "c")) #false)

(define (s-in-los? s los)
  (ormap (λ (los-s) (string=? s los-s))
         los))

; REPLACE ORIG. VERSION W/NEW ONE (COMMENT/UNCOMMENT) AND RERUN
;<DO NOW>



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function connected? that accepts two strings representing two pages
; in a wiki, and a wiki. It should return whether or not there is a path from
; the first page to the second.

; Do the first three steps of the design recipe.

; connected? : String String Wiki -> Boolean
; Determines whether or not the two pages have a path between them
 
(check-expect (connected? "NEU" "Khoury" WIKI-1) #true)
(check-expect (connected? "Boston" "Khoury" WIKI-1) #true)
(check-expect (connected? "New Orleans" "Khoury" WIKI-1) #false)

; This doesn't quite seem to fit the template. Let's think about how to solve it.
;<BRAINSTORM>








; Implement the function. Start with a helper func: (lookup name wiki) -> LoS

; lookup : String Wiki -> [List-of String]
; Returns the links from the page, or the empty list if it is not found

(check-expect (lookup "NOT PRESENT" WIKI-1) '())
(check-expect (lookup "Boston" WIKI-1) (list "NEU" "Shillman Hall"))

(define (lookup page w)
  ; Start from template:
  ; <DO NOW><SCROLL-RM>
  #;
  (cond [(empty? w) ...]
         [(cons? w) (... (wp-temp (first w)) ...
                         (wiki-temp (rest w)) ...)])







  
  (cond [(empty? w) '()]
        [(cons? w) (if (string=? (page-title (first w)) page)
                       (page-links (first w))
                       (lookup page (rest w)))])
  )

; Now work on the main function implementation.
; Phase 1: see if page2 is in page1's links; if so, we're done.

; connected? : String String Wiki -> Boolean
; Determines whether or not the two pages have a path between them

#;
(define (connected? page1 page2 wiki)
  (local [; prefetch all the outgoing linkss
          (define LINKS-FROM-PAGE1 ???)
          ; see if page we want is direct link (hint: use prev. func)
          (define IN-PAGE1-LINKS? (??? LINKS-FROM-PAGE1))]
    (or IN-PAGE1-LINKS?
        ...)))

; Phase 2: handle case that it's not a direct link: We want to recursively search all
; of the pages we *can* get to from current page. Sounds like a combo of connected? and
; a certain list abstraction...

(define (connected? page1 page2 wiki)
  (local [; prefetch all the outgoing linkss
          (define LINKS-FROM-PAGE1 (lookup page1 wiki))
          ; see if page we want is direct link (hint: use prev. func)
          (define IN-PAGE1-LINKS? (s-in-los? page2 LINKS-FROM-PAGE1))]
    (or IN-PAGE1-LINKS?
        ;<DO NOW><SCROLL>







        
        (ormap (λ (p) (connected? p page2 wiki)) LINKS-FROM-PAGE1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Great! Looks like everything works.
; Are we going to call it a day? Of course not! We're going to
; push it a little bit harder!. Try the following:

;(connected? "Khoury" "Shillman Hall" WIKI-1)
;<DO NOW>





; That didn't go so well... Why not? Let's try it in the Stepper.
;


; So we know the specific problem, but what is the general issue?
; One way to think about it is that the wiki is not getting smaller each time.
; With recursion thus far, we've always seen the input, i.e. the size of the
; problem, get smaller with each level of recursion.
; For the next class, meditate on how we might do that with this problem...

