#lang sicp

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

(define (filtered-accumulate combiner null-value term a next b valid?)
  (if (> a b)
      null-value
      (combiner (if (valid? (term a)) (term a) null-value)
                (filtered-accumulate combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b
                                     valid?))))

(define (filter-accumulatei combine null-value term a next b valid?)
  (define (iter i result)
    (cond ((> i b) result)
          ((valid? (term i)) (iter (next i) (combine (term i) result)))
          (else (iter (next i) result))))
  (iter a null-value))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum a b)
  (filtered-accumulate +
                       0
                       (lambda (x) x)
                       a
                       (lambda (x) (+ x 1))
                       b
                       prime?))

(prime-sum 1 10)

(define  (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (coprime? i n)
  (and (< i n)
       (= 1 (gcd i n))))

(define (coprime-sum n)
  (filter-accumulatei *
                       1
                       (lambda (x) x)
                       1
                       (lambda (x) (+ x 1))
                       n
                       (lambda (x) (coprime? x n))))

(coprime-sum 10)