#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulatei combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

(define (sum1 term a next b)
  (accumulatei +
               0
               term
               a
               next
               b))

(define (product1 term a next b)
  (accumulatei *
               1
               term
               a
               next
               b))

(sum1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(product1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)