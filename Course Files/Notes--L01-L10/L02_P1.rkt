;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L02-P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
Today's topics:
- Reeview logistics
  Khoury acct
  Check Canvas and Piazza daily
- Review infix notation
- Review DrRacket
========================================
Moving on...
- Other types of data?
  floating point, fractions, imaginary numbers, ...
  But also strings, images...
- Libraries

- Documentation

- Challenge--sunny skies!

- Challenge 2: draw a sunset, where we can place the sun in any vertical
  position
  Maybe there's a function called locate-image
  No? Let's start with a image composition function we *do* know: overlay
[SCROLL]





(place-image (circle 25 "solid" "yellow") 220 50 (rectangle 300 200 "solid" "light blue"))

- Can we make this simpler? Use "define"...
Learning a language involves learning its syntax and semantics
Each time we pick up a new language feature, we will describe:
- Vocabulary
- Grammar
- Semantics

Define "define":
Vocabulary: define
Grammar: (define NAME VALUE)
            where NAME can be any label. By convention, make it uppercase
            (indicates a constant that does not change). VALUE can be any value,
            or something that evaluates to a value.
Semantics: Using define binds the name to that value,
           allowing you to use the name elsewhere in the program.

- Let's use this new superpower to define SUN and SKY

- Then, let's put SUN in the SKY
  What is the coordinate system for positions??
========================================
- What is a function?
  From mathematics: f(x) = x * 2
  How to do this in BSL?

- So we have another alternative use of define:

Vocab:	 define, same as before.
Grammar: (define (FUNCTION-NAME ARG-NAME ARG-NAME ...) VALUE)
         VALUE can use ARG-NAME inside of it.
Semantics: As before, define binds the function name to that value
         allowing you to call that function elsewhere in your program.

- Let's turn our "f(x) = x * 2" into a BSL function:

- f is not a very meaningful name...

- Let's create a function to compute the area of a rectangle...

- Create a function draw-sun to put our sun at a
  fixed horizontal position, but a specifiable vertical position...
-Try
  (draw-sun 10)
  (draw-sun 20)
  ...

- Try animate...


Challenge for next class:
Create a function draw-eclipse that accepts an x-coordinate and
draws a scene containing a sun (at a static location)
and a moon horizontally aligned with the sun at the given x-coordinate.


