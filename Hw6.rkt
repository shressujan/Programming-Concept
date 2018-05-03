#lang plai

(define (changetoProc x)
  (second (assoc x (list (list '+ +)
                         (list '- -)
                         (list '* *)
                         (list '/ /))))
  )

(define-type FAE
  [ num (n number?)]
  [ binop (op procedure?) (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [if0 (c FAE?) (t FAE?) (e FAE?)]
  [ fun (args(listof symbol?)) (body FAE?)]
  [ app (fun-expr FAE?) (args (listof FAE?))])

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (params (listof symbol?))
             (body FAE?)
             (ds DefrdSub?)])

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub â†’ FAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [ mtSub () (error 'lookup "no binding for identifier" )]
    [ aSub (bound-name bound-value rest-ds)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-ds))]))

;; num+ : numV numV âˆ’â†’ numV
;(define (num+ n1 n2)
; ( numV (+ (numV-n n1) (numV-n n2))))

(define (build-sub-list params args origDS ds)
  (cond[(null? params) ds]
       [#t (let ((param (first params))
                 (arg (first args)))
             ( aSub  param
                     (interp arg origDS)
                     (build-sub-list
                      (rest params)
                      (rest args)
                      origDS
                      ds)))]))

;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) ( numV n)]
    [ binop (b l r) ( numV (b (numV-n (interp l ds)) (numV-n (interp r ds))))]
    [ id (v) (lookup v ds)]
    [if0 (c t e) (cond [(equal? (numV 0) (interp c ds)) (interp t ds)]
                       [#t (interp e ds)]) ]
    [ fun (bound-ids bound-body)
          ( closureV bound-ids bound-body ds)]
    [ app (fun-expr args-expr)
          (local ([define fun-val (interp fun-expr ds)])
            (interp (closureV-body fun-val)
                    ( build-sub-list (closureV-params fun-val)
                                     args-expr
                                     ds
                                     (closureV-ds fun-val))))]))

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       ; modify by parsing the following into a binop
       [(+ - * /) (binop (changetoProc (first sexp))
                         (parse (second sexp))
                         (parse (third sexp)))]
       ; modify by parsing the following into an app with multiple params
       [(with) (app (fun
                     (map first (second sexp))
                     (parse (third sexp)))
                    (map (lambda (x) (parse (second x))) (second sexp)))]
       ; modify by parsing the following to handle multiple params
       [(fun) (fun
               (second sexp)
               (parse (third sexp))             
               )
              ]
       [(if0)(if0
              (parse (second sexp))
              (parse (third sexp))
              (parse (fourth sexp)))]
       
       [else
        (cond ((list? (first sexp))
               ; modify by parsing the following to handle multiple params
               (app (parse (first sexp)) (map (lambda (x) (parse x)) (cdr sexp)))))
        ]
       )]))

;; testing the interpreter on a function application (single param)
(parse '{fun {x y} {+ x y}})
(parse '{{fun {x} {+ x x}} 5})
(interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )


;testing the interpreter on a if0
(interp (parse '(if0 0 1 2)) (mtSub))
(interp (parse '(if0 1 3 2)) (mtSub))

;; sample test cases
(test
 (interp (parse '5 ) [mtSub])
 (numV 5) )

(test
 (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )
 (numV 10))


(interp (parse '{with {{x 2} {y 3}} {with {{z {+ x y}}} {+ x z}}}) (mtSub))
