#lang sicp
(define (p r c)
  (cond ((or (= c 0) (= r c)) 1)
        ((< r c) (error "unvalid col value"))
        (else (+ (p (- r 1) (- c 1))
                 (p (- r 1) c)))))

(define (factorial n)
  (define (fact-iter p c)
    (if (> c n)
        p
        (fact-iter (* c p)
                   (+ c 1))))
  (fact-iter 1 1))

(define (p1 r c)
  (/ (factorial r)
     (* (factorial c)
        (factorial (- r c)))))