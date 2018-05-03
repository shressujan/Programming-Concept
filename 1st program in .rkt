;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1st program in |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#||# ; this is a type of comment that comments out an entire block of code at once
(cond [(< 3 5) true]
      [else false])

(define (square x) (sqr x))

(square 5)
; this is a comment that will help the developer keep information about certain things
(define (meal x) (cond [(< x 8) "breakfast"][else "lunch"]))
(meal 9)


;case for a condition

(define (isfourwheeled wheels)
  (cond[(= wheels 4) "yes four wheeler"]
       [(= wheels 2) "no not a four wheeler"]))
(isfourwheeled 4)

