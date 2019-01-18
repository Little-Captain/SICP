#lang sicp
(define size 2)

size

(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define circumference (* 2 pi radius))
circumference

(* (+ 2
      (* 4 6))
   (+ 3 5 7))