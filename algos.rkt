#lang racket


(define (nest-list f exp n)
  (do ([i 0 (+ i 1)]
       [x exp (f x)]
       [a '() (cons x a)])
    ((>= i n) (reverse a))))

(define (nest-while f exp cnd)
  (do ([x exp (f x)]
       [a '() (cons x a)])
    ((not (cnd x)) (reverse a))))

(define (nest-list-tx f init n txf)
  (do ([i 0 (+ i 1)]
       [cur init (f (txf cur))]
       [a '() (cons cur a)])
    ((= i n) (reverse a))))

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
  (let ([ls (fixed-point-list f exp #:test test #:max max)])
    (if (empty? ls)
        #f
        (first ls))))

(define (repeat f n)
  (do ([i 0 (+ i 1)]
       [accm '() (cons (f) accm)])
    ((= i n) accm)))

(define (insert x ys n)
  (define (insert-aux i ys accm)
    (if (= i n)
        (append (reverse accm) (list x) ys)
        (insert-aux (+ i 1) (cdr ys) (cons (car ys)
                                           accm))))
  (cond
    ((> n (length ys)) ys)
    ((= n (length ys)) (append ys (list x)))
    (else (insert-aux 0 ys '()))))

(define (partial n)
  (λ (f nth-arg)
    (λ args
      (apply f (insert nth-arg args n)))))

(define (n-partial . ns)
  (λ (f . nargs)
    (if (not (= (length ns)
                (length nargs)))
        (error "Cannot create partial function with varying pos and arg lengths")
        (λ args
          (let ((i-args (for/fold ([accm args])
                                  ([p (map cons ns nargs)])
                          (insert (cdr p) accm (car p)))))
            (apply f i-args))))))

(define partial-0 (partial 0))
(define partial-1 (partial 1))
(define partial-2 (partial 2))
(define partial-3 (partial 3))
(define partial-4 (partial 4))
(define partial-5 (partial 5))
(define partial-6 (partial 6))
(define partial-7 (partial 7))
(define partial-8 (partial 8))
(define partial-9 (partial 9))
(define partial-10 (partial 10))

(define (fork-compose f g h)
  (λ args
    (g (apply f args)
       (apply h args))))

(define (id x)
  x)

(define (rook-compose g h)
  (fork-compose id g h))

(provide (all-defined-out))
