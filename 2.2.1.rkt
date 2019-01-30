#lang sicp

(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(cons 5 one-through-four)

; 表操作
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (lengthi items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define odds (list 1 3 5 7))

(lengthi odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append squares odds)

; e.2.17
(define (last-pair items)
  (let ((l (length items)))
    (if (= l 0)
        (error "items length can't be zero")
        (list-ref items (- l 1)))))

(last-pair (list 23 72 149 34))

; 对表的映射
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5 6 7) 10)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x)) (list -10 2.5 -11.6 17))

(define (scale-list1 items factor)
  (map (lambda (x) (* x factor)) items))
(scale-list1 (list 1 2 3 4 5 6 7) 10)