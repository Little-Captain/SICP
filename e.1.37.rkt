#lang sicp

(define (cont-frac N D k)
  (define (cf i)
    (if (= k i)
        (/ (N k) (D k))
        (/ (N i)
           (+ (D i) (cf (+ i 1))))))
  (cf 1))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)
; 迭代版本