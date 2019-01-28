#lang sicp

;(define (linear-combination a b x y)
;  (+ (* a x) (* b y)))

;(linear-combination 1 2 3 4)

(define add +)
(define mul *)

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

(linear-combination 1 2 3 4)