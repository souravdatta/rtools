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

(define (fixed-point-list f exp #:test [test equal?] #:max [max 0])
  (do ([x exp (f x)]
       [i 0 (+ i 1)]
       [a '() (cons x a)])
    ((or (and (> max 0)
              (> i max))
         (and (> (length a) 1)
              (test (first a)
                    (second a))))
     (if (and (> max 0) (> i max))
         #f
         a))))

(define (fixed-point f exp #:test [test equal?] #:max [max 0])
  (let ([ls (fixed-point-list f exp #:test test)])
    (if (empty? ls)
        #f
        (first ls))))



(provide (all-defined-out))
