#lang sicp
(define (even? n)
  (= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

;(define (* a b)
;  (cond ((= b 0) 0)
;        ((even? b) (* (double a) (halve b)))
;        (else (+ a (* (double a) (halve (- b 1)))))))
(define (* a b p)
  (cond ((= b 0) p)
        ((even? b) (* (double a) (halve b) p))
        (else (* a (- b 1) (+ p a)))))

(define (mult a b)
  (* a b 0))

(mult 12 12)
(mult 11 11)
(mult 13 13)
(mult 3 13)
(mult 4 12)
