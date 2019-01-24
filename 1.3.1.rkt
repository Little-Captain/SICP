#lang sicp
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;(define (<name> a b)
;  (if (> a b)
;      0
;      (+ (<term a>)
;         (<name> (<next> a) b))))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n))
(define (sum-cubes1 a b)
  (sum cube a inc b))

(display "-------------------------\n")

(sum-cubes 1 2)
(sum-cubes 1 20)
(sum-cubes 1 30)
(sum-cubes1 1 2)
(sum-cubes1 1 20)
(sum-cubes1 1 30)

(display "-------------------------\n")

(define (identity x) x)
(define (sum-integers1 a b)
  (sum identity a inc b))

(sum-integers 1 10)
(sum-integers1 1 10)

(display "-------------------------\n")

(define (pi-sum1 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 10000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(define (integral1 f a b n)
  (define (h) (/ (- b a) n))
  (define (k x) (+ x 1))
  (define (ak k) (cond ((or (= k 0) (= k n)) 1)
                       ((= (remainder k 2) 0) 2)
                       (else 4)))
  (define (fk k) (* (ak k) (f (+ a (* k (h))))))
  (* (sum fk 0 k n) (/ (h) 3)))

(integral1 cube 0 1 100)
(integral1 cube 0 1 1000)