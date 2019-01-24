#lang sicp

(define (cont-frac N D k)
  (define (cf i)
    (if (= k i)
        (/ (N k) (D k))
        (/ (N i)
           (+ (D i) (cf (+ i 1))))))
  (cf 1))

(define (square x) (* x x))

(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
        x
        (- (square x))))
  (define (D i)
    (- (* i 2) 1))
  (exact->inexact (cont-frac N D k)))

(tan 10)
(tan-cf 10 100)