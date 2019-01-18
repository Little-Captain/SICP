#lang sicp
(define (square x) (* x x))
(define (rate x y) (/ (abs (- x y)) x))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.00001))
(define (good-enough? guess new-guess)
  (< (rate guess new-guess) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (sqrt-iter (improve guess x)
                 x)))

; 使用 let 结构
(define (sqrt-iter1 old-guess x)
  (let ((new-guess (improve old-guess x)))
    (if (good-enough? old-guess new-guess)
        new-guess
        (sqrt-iter new-guess x))))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt1 x)
  (sqrt-iter1 1.0 x))