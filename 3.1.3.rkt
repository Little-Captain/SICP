#lang racket

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

(W 20)
(W 10)

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(D 20)
(D 10)

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))

(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))

(W1 20)
(W1 20)

(W2 20)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial-new n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          ; product counter 的赋值顺序不能变。
          ; 赋值顺序常常是带有赋值的程序需要考虑的问题。
          ; 函数式程序设计中不会出现这类问题。
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))