;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname L1_P1-trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; TREES AND MUTUAL RECURSION
;
; Today, we will move on from linear lists to branching trees!

; Motivating example:
; How would we represent a family tree as a data design? We're assuming
; a simple model, where each person has two biological parents.

(define-struct person [name parent1 parent2])
 
; A Person is a (make-person String String String)

; Seems a bit underpowered. How about:

; A Person is a (make-person String Person Person)

; Now, we have the beginnings of a way to define trees of structures: like a list,
; but with branching at any element. A tree is a very important data type in computer science.

; What are some potential pitfalls to this design?
; - It is doubly-self-referential--will that be a problem??
; - We can't actually construct an example (remember our first attempts at self-referential
; data?) How did we solve it then?

; We need a way to gracefully end a branch, similar to needing a way to end a list.
; For a family tree, it would indicate "parent not known"


; A Person is one of:
; - #false
; - (make-person String Person Person)
; Interpretation:  A person's name and two biological parents.
; Or a sentinel value representing that a person's identity/lineage is not known.

; This is called a binary tree because it has two self-references.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create some examples

(define PERSON-0 #false)
(define PERSON-1 (make-person "Alice" PERSON-0 PERSON-0))
(define PERSON-2 (make-person "Bob" PERSON-0 PERSON-0))
(define PERSON-3 (make-person "Carol" PERSON-1 PERSON-2))

; Lastly, the template:

(define (person-temp p)
  (...
   ;<DO NOW><SCROLL>







   
   (cond [(boolean? p) ...]
         [(person? p) (... (person-name p) ...
                           (person-temp (person-parent1 p)) ...
                           (person-temp (person-parent2 p)) ...)])))

; Note: multiple self-referential fields leads to multiple recursive calls,
; once for each parent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function tree-size that takes in a person and returns
; the number of named people in the tree.

; tree-size : Person -> NatNum
; Returns the number of non-false people in the tree
#|
(check-expect (tree-size PERSON-0) ???)
(check-expect (tree-size PERSON-1) ???)
(check-expect (tree-size PERSON-2) ???)
(check-expect (tree-size PERSON-3) ???)
|#
(define (tree-size p)
  ; Start from template:
  ; <DO NOW><SCROLL>
  #;
  (cond [(boolean? p) ...]
         [(person? p) (... (person-name p) ...
                           (person-temp (person-parent1 p)) ...
                           (person-temp (person-parent2 p)) ...)])








  (cond [(boolean? p) 0]
        [(person? p) (+ 1
                        (tree-size (person-parent1 p))
                        (tree-size (person-parent2 p)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function tree-depth that takes in a person and returns
; the deepest path in the tree.

; tree-depth : Person -> NatNum
; Returns the longest "path" in the tree
#|
(check-expect (tree-depth PERSON-0) ???)
(check-expect (tree-depth PERSON-1) ???)
(check-expect (tree-depth PERSON-2) ???)
(check-expect (tree-depth PERSON-3) ???)
|#
(define (tree-depth p)
  ; Start from template:
  ; <DO NOW><SCROLL>
  #;
  (cond [(boolean? p) ...]
         [(person? p) (... (person-name p) ...
                           (person-temp (person-parent1 p)) ...
                           (person-temp (person-parent2 p)) ...)])








  (cond [(boolean? p) 0]
        [(person? p) (add1 (max (tree-depth (person-parent1 p))
                                (tree-depth (person-parent2 p))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MORE "BRANCHY" TREES
; A 3-ary ("ternary") tree is one where every non-empty node has exactly 3 children.
; Design a data definition 3-tree that represents this, with each node having a number inside of it:

(define-struct tree [val c1 c2])

; A tree is one of:
; - #f
; - (make-binary-tree X binary-tree binary-tree)
; Interpretation: A X and two children.
 
(define TREE-1 #f)
(define TREE-2 (make-tree 2 TREE-1 TREE-1 TREE-1))
(define TREE-3 (make-tree 3 TREE-2 TREE-1 TREE-1))

(define (tree-temp tree)
  (...
   (cond [(boolean? tree) ...]
         [(tree tree) (... (tree-val tree) ...
                                (tree-temp (tree-c1 tree)) ...
                                (tree-temp (tree-c2 tree)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sometimes, we want to represent a tree that has a variable number of children per node.
; How would we represent that?
; Think about (say) links on a webpage; not all web pages have the same number.
; Thus, we need a way to represent that.
;<BRAINSTORM>








; Some ideas:

#;
(define-struct page [title link1 link2 link3 ... link99])
 
; A WebPage is a (make-page String WebPage ... WebPage)
; Interpretation: A web page's title and links to other pages.

; Pitfalls:
; - We'd need a way to indicate that not all of the links are active;
;   but we can solve that.
; - More importantly, we'd have to cap the number of links--not good!

; Any ideas?
;<BRAINSTORM>







; Instead, we might do...

(define-struct page [title links])
 
; A WebPage is a (make-page String [List-of WebPage])
; Interpretation: A web page's title and links to other pages.

; Let's create some data examples...

(define PAGE-0 (make-page "Khoury" '()))
(define PAGE-1 (make-page "NEU" (list PAGE-0)))
(define PAGE-2 (make-page "Boston" (list PAGE-0 PAGE-1)))

; Let's design its template:

(define (wp-temp wp)
  (... (page-title wp) ...
       (lop-temp (page-links wp)) ...))
 
(define (lop-temp lop)
  (...
   (cond [(empty? lop) ...]
         [(cons? lop) (... (wp-temp (first lop)) ...
                           (lop-temp (rest lop)) ...)])))

; Draw arrows between these two templates - what do you notice?
; This is called mutual recursion.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function exists? that accepts a webpage and a string,
; and determines whether a page with that title exists.

; exists? : WebPage String -> Boolean
; Determines whether or not a page exists with the specified title
 
(check-expect (exists?/v1 PAGE-0 "Foo") #false)
(check-expect (exists?/v1 PAGE-0 "Khoury") #true)
(check-expect (exists?/v1 PAGE-2 "Khoury") #true)
(check-expect (exists?/v1 PAGE-2 "Boston") #true)
(check-expect (exists?/v1 PAGE-2 "Other") #false)
 
(define (exists?/v1 page str)
  ; Start from template:
  ; <DO NOW><SCROLL> helper: exists-lop?/v1
  #;
  (... (page-title wp) ...
       (lop-temp (page-links wp)) ...)








  (or (string=? (page-title page) str)
      (exists-lop?/v1 (page-links page) str)))
 
; exists-lop? : [List-of WebPage] String -> Boolean
; Determines whether or not a page with that title exists in the list
 
(check-expect (exists-lop?/v1 (list PAGE-0) "Foo") #false)
(check-expect (exists-lop?/v1 (list PAGE-0 PAGE-2) "Khoury") #true)

(define (exists-lop?/v1 lop str)
  ; Start from template:
  ; <DO NOW><SCROLL> helper: exists-lop?/v1
  #;
  (cond [(empty? lop) ...]
         [(cons? lop) (... (wp-temp (first lop)) ...
                           (lop-temp (rest lop)) ...)])







  (cond
    [(empty? lop) #false]
    [(cons? lop) (or (exists?/v1 (first lop) str)
                     (exists-lop?/v1 (rest lop) str))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What do you notice about that second function?
; Could we write that function using a list abstraction?
; exists-lop? : [List-of WebPage] String -> Boolean
; Determines whether or not a page with that title exists in the list
(check-expect (exists-lop? (list PAGE-0) "Foo") #false)
(check-expect (exists-lop? (list PAGE-0 PAGE-2) "Khoury") #true)
(define (exists-lop? lop str)
  (ormap (λ (wp) (exists?/v2 wp str)) lop))

; Same as before. but calls our new version:
(check-expect (exists?/v2 PAGE-0 "Foo") #false)
(check-expect (exists?/v2 PAGE-0 "Khoury") #true)
(check-expect (exists?/v2 PAGE-2 "Khoury") #true)
(check-expect (exists?/v2 PAGE-2 "Boston") #true)
(check-expect (exists?/v2 PAGE-2 "Other") #false)
(define (exists?/v2 page str)
  (or (string=? (page-title page) str)
      (exists-lop? (page-links page) str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's see if we can improve upon it...
;<BRAINSTORM> Hint: local, anybody?









(check-expect (exists?/v3 PAGE-0 "Foo") #false)
(check-expect (exists?/v3 PAGE-0 "Khoury") #true)
(check-expect (exists?/v3 PAGE-2 "Khoury") #true)
(check-expect (exists?/v3 PAGE-2 "Boston") #true)
(check-expect (exists?/v3 PAGE-2 "Other") #false)
(define (exists?/v3 page str)
  (local [; exists-lop? : [List-of WebPage] -> Boolean
          ; Determines whether or not a page with that
          ; title exists in the list
          (define (exists-lop? lop)
            (ormap (λ (wp) (exists?/v3 wp str)) lop))]
    (or (string=? (page-title page) str)
        (exists-lop? (page-links page)))))
; By moving exists-lop? into local, it implicitly has access to str--one less arg to pass
; BTW, note that our local function has same name as our earlier global function--
; not a problem!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; As one final refinement, we can remove some additional complexity by making
; two mutual helper functions: exists-page? and exists-lop?.
; Notice that new local function exists-page? shares the scope of the outer function,
; too, just like exists-lop?, so it, also, no longer needs to be called w/str.
; There is another improvement: since exists-lop? doesn't have to call exists? (now
; exists-page?) w/str, it doesn't even need a lambda--can just call ormap w/exists-page? !
(define (exists? page str)
  (local [; exists-page? : WebPage -> Boolean
          ; Does the title exist in the page?
          (define (exists-page? wp)
            (or (string=? (page-title wp) str)
                (exists-lop? (page-links wp))))
 
          ; exists-lop? : [List-of WebPage] -> Boolean
          ; Determines whether or not a page with that
          ; title exists in the list
          (define (exists-lop? lop)
            (ormap exists-page? lop))]
    (exists-page? page)))

(check-expect (exists? PAGE-0 "Foo") #false)
(check-expect (exists? PAGE-0 "Khoury") #true)
(check-expect (exists? PAGE-2 "Khoury") #true)
(check-expect (exists? PAGE-2 "Boston") #true)
(check-expect (exists? PAGE-2 "Other") #false)
