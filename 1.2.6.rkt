#lang sicp
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

(prime? 100)
(prime? 130)
(prime? 101)
(prime? 103)
(prime? 971)
(prime? 981)
(prime? 979)
(prime? 1971)
(prime? 20971)

; e.1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(display "-------------------\n")

(fast-prime? 100 10)
(fast-prime? 130 10)
(fast-prime? 101 10)
(fast-prime? 103 10)
(fast-prime? 971 10)
(fast-prime? 981 10)
(fast-prime? 979 10)
(fast-prime? 1971 10)
(fast-prime? 20971 10)

; e.1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (next-odd n)
  (if (odd? n)
      (+ 2 n)
      (+ 1 n)))

(define (continue-primes n count)
  (cond ((= count 0) (display "are primes."))
        ((prime? n) (display n)
                    (newline)
                    (continue-primes (next-odd n) (- count 1)))
        (else (continue-primes (next-odd n) count))))

(continue-primes 1000 3)
(display "\n")
(continue-primes 1000 13)

(define (search-for-primes n)
  (let ((start-time (runtime)))
    (continue-primes n 3)
    (- (runtime) start-time)))

(search-for-primes 100)
