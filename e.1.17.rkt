#lang sicp
;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))
(define (even? n)
  (= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* (double a) (halve (- b 1)))))))

(* 12 3)
(* 13 9)
(* 12 12)
(* 13 13)
(* 11 11)