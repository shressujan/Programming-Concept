#lang racket

; Problem 1
(define (kilometers? x) (* x 1.609))
(define (miles? x) (* x 0.62156))

(displayln "Problem 1")
(displayln "kilometers")
(kilometers? 5)
(displayln "Miles")
(miles? 8)

;Problem 2

;part a
(displayln "")
(displayln "Problem 2")
(define (foo x y) 
   (+ x y (- x)))
(displayln "Part a")
(foo (- 2) 4)

;part b
(define (a b c d)
   (e b c d))

(define f +) 

(define e f)
(displayln "Part b")
(a (+ 3 2) (* 4 5) 3)

;part c
(define (foo2 b c d)
   ((foo3 (foo4 b)) c d))

(define (foo3 a) 
   a)

(define (foo4 a)
   (if (positive? a) 
       +
       -))
(displayln "Part c")
(foo2 1 (+ 2 3) 4)

;Problem 3

(displayln "")
(displayln "Problem 3")

(define (p x)
  (define (q y)
    y)
  (q 1))
(p 2)

(define (baz n)
  (define (bar a b)
    (define (foo a)
      (* a (+ a 2)))
     (if (= a 14)
       b
       (bar (+ a 1) (+ (foo a) b))))
  (bar 3 n))
(displayln " Lexical Scoping")
(baz 2)

;Problem 4

(displayln "")
(displayln "Problem 4")

(define (sum-range x y)
  (define sum 0)
  (define (add x y sum)
    (cond[(<= x y) (add (+ x 1) y (+ sum x)) ]
         [else  sum]))
  (cond [(< x y) (add x y 0)]
        [(> x y) (add y x 0)]
        [else "they are equal"]))

(sum-range 6 6);
(sum-range 1 50);
(sum-range 5 2);

