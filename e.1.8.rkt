#lang sicp
(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (rate x y) (/ (abs (- x y)) x))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.00001))
(define (good-enough? guess new-guess)
  (< (rate guess new-guess) 0.001))

(define (s-iter old-guess x)
  (let ((new-guess (improve old-guess x)))
    (if (good-enough? old-guess new-guess)
        new-guess
        (s-iter new-guess x))))

(define (siter x)
  (s-iter 1.0 x))

(siter 8)
(siter 8000)
(siter 0.001)