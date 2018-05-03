#lang racket
;problem 4 with recursion
(displayln "")

(define (range x y)
  (if(> x y) 0
  (+ x (range (+ x 1) y))))

(range 1 5);

;finding the factorial
(define (factorial n)
  (cond [(< n 2) 1]
        [else (* n (factorial (- n 1)))]));
(factorial 3);

(modulo 344 100);

(equal? 1 1);

(truncate (/ 264 10));

(list 1 2 4);