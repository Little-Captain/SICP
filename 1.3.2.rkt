#lang sicp
; lambda
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(integral (lambda (x) (* x x x x)) 0 1 0.001)

(define (plus4 x) (+ x 4))
(define plus_4 (lambda (x) (+ x 4)))

(plus4 6)
(plus_4 6)

((lambda (x y z) (+ x y ((lambda (x) (* x x)) z))) 1 2 3)

; let
(define (f x y)
  (define (f-helper a b)
    (+ (* x ((lambda (x) (* x x)) a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (fl x y)
  ((lambda (a b)
     (+ (* x ((lambda (x) (* x x)) a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (flet x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x ((lambda (x) (* x x)) a))
       (* y b)
       (* a b))))

(define (fd x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x ((lambda (x) (* x x)) a))
     (* y b)
     (* a b)))

(f 10 11)
(fl 10 11)
(flet 10 11)
(fd 10 11)