#lang sicp

(define (cont-frac N D k)
  (define (cf i)
    (if (= k i)
        (/ (N k) (D k))
        (/ (N i)
           (+ (D i) (cf (+ i 1))))))
  (cf 1))

(+ (exact->inexact (cont-frac (lambda (x) 1)
                              (lambda (x) (if (= 0 (remainder (+ x 1) 3))
                                              (* 2 (/ (+ x 1) 3))
                                              1))
                              1000))
   2)
