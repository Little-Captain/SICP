#lang sicp
(define (min x y)
  (cond ((< x y) x)
        (else y)))
(define (sum-of-max x y z)
  (- (+ x y z)
     (min x
          (min y z))))
(sum-of-max 10 9 8)