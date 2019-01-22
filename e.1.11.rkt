#lang sicp
(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))

(f1 0)
(f1 1)
(f1 2)
(f1 3)
(f1 4)
(f1 5)
(f1 6)
(f1 7)
(f1 8)
(f1 9)

(define (f2 n)
  (define (f-iter a b c i)
    (if (= i n)
        c
        (f-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (+ i 1))))
  (f-iter 2 1 0 0))

(f2 0)
(f2 1)
(f2 2)
(f2 3)
(f2 4)
(f2 5)
(f2 6)
(f2 7)
(f2 8)
(f2 9)