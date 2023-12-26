#lang racket


(define (nest-list f exp n)
  (do ([i 0 (+ i 1)]
       [x exp (f x)]
       [a '() (cons x a)])
    ((>= i n) a)))

(define (nest-while f exp cnd)
  (do ([x exp (f x)]
       [a '() (cons x a)])
    ((not (cnd x)) a)))

(define (nest f exp n)
  (do ([i 0 (+ i 1)]
       [x exp (f x)])
    ((>= i n) x)))

(define (fixed-point-list f exp #:test [test equal?])
  (do ([x exp (f x)]
       [a '() (cons x a)])
    ((and (> (length a) 1)
          (test (first a)
                (second a)))
     a)))

(define (fixed-point f exp #:test [test equal?])
  (let ([ls (fixed-point-list f exp #:test test)])
    (if (empty? ls)
        #f
        (first ls))))



(provide (all-defined-out))
