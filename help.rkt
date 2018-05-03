#lang racket
;Question 1a
(display "1a)")(newline)
(define (compare p1 p2 n)
  (if (= (p1 n) (p2 n))
     #t
     #f))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(display "Test (compare square cube 2) should be false: ")
(compare square cube 2) 
(display "Test (compare square cube 1) should be true: ")
(compare square cube 1) 

;Question 1b
(display "1b)")(newline)
(define (make-comparator p1 p2)
  (define (new-comparator n)
    (compare p1 p2 n))
  new-comparator)
(display "Test (compare -sc) should be true: ")
(define compare-sc 
  (make-comparator square cube))
(compare-sc 2)

;Question 1c
(display "1c)")(newline)
(define (test comparator from to)
  (if (comparator from)
      (if (= from to)
          #t
          (test comparator (+ from 1) to))
      #f))
(display "Test (test compare-sc 1 3) should be false: ")
(test compare-sc 1 3) ;; tests compare-sc with 1, 2 and 3

;Question 1d
(display "1d)")(newline)
(define (make-test from to)
  (define (generate-test comp)
    (test comp from to))
  generate-test)
(define test-1-3
  (make-test 1 3)) ;; a test with 1, 2 and 3 as test values
(display "Test (test-1-3 compare sc) should be false: ")
(test-1-3 compare-sc)

;Question 2a
(display "2a)")(newline)
(define (complement proc)
  (define (new-proc n)
    (if (proc n)
      #f
      #t))
  new-proc)
(define (some-proc n)
  (if (= n 1)
      #t
      #f))
(display "Test (some-proc 1) should be true: ")
(some-proc 1) 
(display "Test ((complement some-proc) 1) should be false: ")
((complement some-proc) 1) 
(define (odd? n)
  ((complement even?) n))
(display "Test (even? 5) should be false: ")
(even? 5)
(display "Test (odd? 5) should be true: ")
(odd? 5)

;Question 2b
(display "2b)")(newline)
(define x 5)
(define expression1 
  (lambda (a b c) (+ (* a (square x)) (* b x) c)))
(display "Test lambda function, output should be 38: ")
(expression1 1 2 3)
(display "Test let function, output should be 38: ")
(let ((a 1) (b 2) (c 3))
  (+ (* a (square x)) (* b x) c))

;Question 2c
(display "2c)")(newline)
(display "Test original function with the bug, returns 10 due to x already being defined as 5 from 2b: ")
(let ((x 4) 
      (y (+ x 1))) 
  (+ x y))
(display "Updated function to a let*. Should output 9: ") 
(let* ((x 4)
      (y (+ x 1)))
  (+ x y))

;Question 2d
(display "2d)")(newline)
(display "Test original function, should output 24: ")
(let ((+ -) 
      (- +) 
      (* /) 
      (/ *)) 
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2))
(display "Test new let*, should output 24: ")
(let* ((a +)
      (+ -) 
      (- a)
      (b *)
      (* /) 
      (/ b)) 
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2))

;Question 2e
(display "2e)")(newline)
(define (iffy test consequent alternative)
  (cond ((eq? test #t) consequent)
        ((eq? test #f) alternative)))
(display "Created iffy, testing problem 1 recreated using iffy. ")(newline)

(define (compareN p1 p2 n)
  (iffy (= (p1 n) (p2 n))
     #t
     #f))
(display "Should be false: ")
(compareN square cube 2) 
(display "Should be true: ")
(compareN square cube 1) 
(define compare-scN
  (make-comparator square cube))
(display "Should be true: ")
(compare-scN 1)
(display "Should be false: ")
(test compare-scN 1 3)
(define test-1-3N
  (make-test 1 3))
(display "Should be false: ")
(test-1-3N compare-scN)
(display "This seems to be an acceptable replacement for the special form if.")