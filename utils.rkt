#lang racket

(struct not-found ())

(define (chain-get hash path #:default [default #f])
  (define (chain-get-aux h paths last-val)
    (cond
      ((null? paths) last-val)
      ((not (hash? h)) default)
      (else (let ([v (hash-ref h (car paths) (not-found))])
              (cond
                ((not-found? v) default)
                (else (chain-get-aux v (cdr paths) v)))))))
  (chain-get-aux hash path default))


(provide chain-get)
