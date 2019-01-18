#lang sicp
; (cond (<p1> <e1>)
;       (<p2> <e2>)
;       ...
;       (<pn> <en>))
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs1 x)
  (cond ((< x 0) (- x))
        (else x)))

; (if <predicate> <consequent> <alternative>)
(define (abs2 x)
  (if (< x 0)
      (- x)
      x))

; (and <e1> ... <en>)
; (or <e1> ... <en>)
; (not <e>)
(define (range_5_10 x)
  (and (> x 5) (< x 10)))

;(define (>= x y)
;  (or (> x y) (= x y)))
(define (>= x y)
  (not (< x y)))