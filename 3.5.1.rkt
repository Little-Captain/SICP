#lang sicp

(define (prime? x)
  ; 判断是否是素数
  true)

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

;(define (sum-primes1 a b)
;  (accumulate +
;              0
;              (filter prime? (enumerate-interval a b))))