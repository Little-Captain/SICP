#lang sicp

(define (cube n) (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sumi term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral1 f a b n)
  (define (h) (/ (- b a) n))
  (define (k x) (+ x 1))
  (define (ak k) (cond ((or (= k 0) (= k n)) 1)
                       ((= (remainder k 2) 0) 2)
                       (else 4)))
  (define (fk k) (* (ak k) (f (+ a (* k (h))))))
;  (* (sum fk 0 k n) (/ (h) 3)))
  (* (sumi fk 0 k n) (/ (h) 3)))

(integral1 cube 0 1 100)
(integral1 cube 0 1 1000)