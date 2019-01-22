#lang sicp
(define  (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 40 8)
(gcd 8 40)
(gcd 8 8)
(gcd 0 5)
(gcd 206 40)