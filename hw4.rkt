#lang racket

(or #t #t)
(displayln "Problem 1")
(displayln "Part a");

(define (contains? list object)
  (cond [(null? list) "Value:#f"]
        [(equal? (car list) object) "Value: #t"]
        [else (contains? (cdr list) object)]));

(displayln "Using recursion");
(display "value: " );
( contains? (list 1 2 3 4) 4);


(displayln "Using NO recursion");


(define (accumulate operator null list)
  (if (null? list)
      null
      (operator (car list)
                (accumulate operator null (cdr list)))))

(define (contains2? list object)
  (accumulate (lambda (x y)
                (or x y)) #f (map(lambda(x)
                                   (equal? x object))
                                 list)));
(display "value: " );
(contains2? (list 1 2 3 2 4) 2)


(newline);

(displayln "Part b");

(define (remove object list)
  (cond [(null? list) null]
        [(equal? (car list) object) (remove object (cdr list) )]
        [else (cons (car list)(remove object (cdr list) ))]));

(displayln "with recursion");
(display "value: " );
(remove  1 (list 1 2 3 2 1));

(displayln "without recursion");

(define (remove2 object list)
  (accumulate (lambda (item rest)
                (cond [ (equal? object item) rest]
                      [#t (cons item rest)]))
              null
              list));
(display "value: " );
(remove2  1 (list 1 2 3 2 1));



(newline)
(displayln"Problem 2")

;; 1 + (2 / (3 * 5 + 1)) + (-4)
(define expr
  (list +
        1
        (list /
              2
              (list +
                    (list * 3 5)
                    1))
        (list - 0 4)));
expr
;Value: (+ 1 (/ 2 (+ (* 3 5) 1)) (- 0 4))
(displayln "part a")
(define (operator? object)
  (cond[(equal? object +)]
       [(equal? object -)]
       [(equal? object *)]
       [(equal? object /)]
       [#t #f]));
(display "value: " );
(operator? (car expr)) ;
(newline);
(displayln "part b")

(define (null-val operator)
  (cond[(equal? operator +) 0]
       [(equal? operator -) 0]
       [(equal? operator *) 1]
       [(equal? operator /) 1]
       [#t null]));
(display "value: " );
(null-val *);
(newline);
(displayln "part c");

#|
(define (expression? object)
  (cond[ (list? object)
       (and
          (accumulate (lambda (x y)
                (or x y)) #f (map(lambda(x)
                           (operator?  x))
                         object))
              (accumulate (lambda (x y)
                (or x y)) #f (map(lambda(x)
                           (number?  x))
                         object)))]
       [#t #f]));

|#

(define (expression? object)
  (cond [(number? object) #t]
        [#t (and (list? object)
                 (> (length object) 2)
                 (operator? (car object))
                 (andmap expression? (rest object)))]))
(display "value: " );
(expression? expr)
(display "value: " );
(expression? (list + 1 2));
(display "value: " );
(expression? (list + 1 ));

(newline);
(displayln "part d");
(define (count-operators expre)
  (cond [(list? expre) (accumulate + 0 (map count-operators expre))]
        [(operator? expre) 1]
        [#t 0]));

(displayln "(count-operators expr) ")
(display "value: " );
(count-operators expr);

(define (count-primitive-operands expre)
  (cond [(list? expre) (accumulate + 0 (map count-primitive-operands expre))]
        [(number? expre) 1]
        [#t 0]));

(displayln "(count-primitive-operands expr)")
(display "value: " );
(count-primitive-operands expr);
(newline);
(displayln "part e")


(define (evaluate expr)
  (cond [(number? expr) expr]
        [(expression? expr) (apply (car expr) (map (lambda (x)
                                                     (cond [(expression? x) (evaluate x)]
                                                           [#t x])) (rest expr)))]
        ))
(display "value: " );
(evaluate expr)


