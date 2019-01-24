#lang sicp

(define (product item a next b)
  (if (> a b)
      1
      (* (item a)
         (product item (next a) next b))))

(define (producti item a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (item a)))))
  (iter a 1))

(define (pi n)
  (define (next n) (+ n 2))
  (define (item n) (/ (* (- n 1) (+ n 1)) (* n n)))
  (exact->inexact (* 4 (product item 3 next n))))

(pi 9)
(pi 99)
(pi 999)

(define (factorial n)
  (define (next n) (+ n 1))
  (define (item n) n)
  (cond ((< n 0) (error "error"))
        ((= n 0) 1)
        (else (producti item 1 next n))))

(factorial 10)