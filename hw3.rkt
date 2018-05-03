#lang racket

(displayln "Problem1")
(displayln "part a");

(define (compare p1 p2 n)
  (define (funct p x)
    (p x))
  (equal? (funct p1 n)  (funct p2 n)));

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(compare square cube 2);
(compare even? odd? 2);

;Probelm 1b
(displayln "part b");


(define (make-comparator p1 p2)
  (define (comparator n)
    (define (funct p x)
      (p x))
    (equal? (funct p1 n)  (funct p2 n)))
  comparator);

(define compare-sc 
  (make-comparator square cube))
(compare-sc 2);

;Probelm 1c
(displayln "part c");

(define (test comparator from to)
  (define (cmp from)
    (cond [(<= from to) (comparator from)]
          [else #f]))
  (cond[(cmp from) (test comparator (+ from 1) to)]
       [else #f]));

(display "Test (test compare-sc 1 3) should be false: ")
(test compare-sc 1 3) ;; tests compare-sc with 1, 2 and 3

;Problem 2

;part 2a

(displayln "part 2a");

(define (isOdd? n)
    (odd? n));
(displayln "Value should be true");
(isOdd? 3);

(define (complement proc)
  (define (some-proc n)
    (not (isOdd? n)))
  some-proc);

(displayln "Value should be false");
((complement isOdd?) 3);

;part 2b
(displayln "part 2b");
(displayln "Using lambda");
(define x 5)
((lambda (a b c)
   (+ (* a (* x x)) (* b x) c))
   1 2 3);
(displayln "Using Let");

(let ([a 1]
      [b 2]
      [c 3])
  (+ (* a (* x x)) (* b x) c));

;part 2c
(displayln "part 2c");
(displayln "Used let* instead of let to bound the local variable sequentially and not simultaneously");
(let* ([x 4]
      [y (+ x 1)]) 
  (+ x y));



;part 2d
(displayln "part 2d using let ");
(let ((+ -)           
      (- +) 
      (* /) 
      (/ *)) 
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2));

(displayln "Used let* should give the same answer as above");
(let* ([x +]
      (+ -) 
      (- x)  
      (y *)
      (* /) 
      (/ y)) 
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2));

;part 2e
(displayln "part 2e");

(define (iffy test consequent alternative)
  (cond [test consequent]
        [#t alternative]))

(define (isEven num)
  (iffy (even? num) #t #f));

(displayln "isEven using iffy for 2")
(isEven 2);
(displayln "isEven using iffy for 3")
(isEven 3);

