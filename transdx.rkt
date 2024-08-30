#lang racket

(struct discarded [x])

(define (redux r x sofar conj #:next [next #f])
  (let ([v (r x sofar conj)])
    (cond
      ((discarded? (first v)) v)
      (else (if next
                (next (first v)
                      (second v))
                (list (first v)
                      (conj (second v)
                            (first v))))))))

(define (mapping f)
  (λ (red)
    (λ (x sofar conj)
      (redux red x sofar conj #:next (λ (y nsofar)
                                       (list (f y)
                                             nsofar))))))

(define (filtering f)
  (λ (red)
    (λ (x sofar conj)
      (redux red x sofar conj #:next (λ (y nsofar)
                                       (if (f y)
                                           (list y
                                                 nsofar)
                                           (list (discarded y)
                                                 nsofar)))))))

(define (idr)
  (λ (x sofar conj)
    (list x sofar)))

(define (comping rs)
  (cond
    ((empty? rs) (idr))
    (else ((first rs) (comping (rest rs))))))

(define (just r)
  (r (idr)))

;; (define (transduce-list source r #:into [into '()])
;;   (for/fold ([accm into])
;;             ([i source])
;;     (second
;;      (redux r i accm list-conj))))


(define (list-conj lst x)
  (cons x lst))

(define (vec-conj v x)
  (vector-append v (vector x)))

(define (transduce src
                   nextf
                   next-srcf
                   eolf
                   rx
                   #:into [into '()]
                   #:conj [conj list-conj])
  (let looper ([accm into]
               [s src])
    (if (eolf s)
        accm
        (looper (second
                 (redux rx (nextf s) accm conj))
                (next-srcf s)))))

(define (transduce-list src r)
  (transduce src
             first
             rest
             empty?
             r))

(define (transduce-vector src r)
  (transduce src
             (λ (v) (vector-ref v 0))
             (λ (v) (vector-drop v 1))
             vector-empty?
             r
             #:into #[]
             #:conj vec-conj))

(define (transduce-file f r)
  (call-with-input-file f
    (λ (in)
      (transduce (read-line in)
                 string->number
                 (λ (_) (read-line in))
                 eof-object?
                 r))))


;; Examples

;; (define t1 (mapping (λ (x) (* x 3))))
;; (define t2 (filtering (λ (x) (= (remainder x 2) 0))))
;; (define t3 (filtering (λ (x) (< x 100))))
;; (define t4 (mapping identity))
;; 
;; (define tx (comping (list t1 t2)))
;; (redux (just t2) 4 '() list-conj)
;; (redux (comping (list t1 t1 t2)) 4 '() list-conj)
;; (redux (comping (list t3 t1 t2)) 4 '() list-conj)
;; 
;; (define algo (comping (list (filtering (λ (x) (< x 100)))
;;                             (mapping (λ (x) (* x 3)))
;;                             (filtering odd?))))
;; 
;; 
;; (define (transduce-rand n r)
;;   (let ([accm '()])
;;     (for ([i (range n)])
;;       (set! accm (second
;;                   (redux r (random 200) accm list-conj))))
;;     accm))
;; 
;; (define (prep-file f)
;;   (call-with-output-file f
;;     (λ (out)
;;       (for ([i (range 20)])
;;         (writeln (random 110) out)))
;;     #:exists 'replace))
;; 
;; (transduce-list (range 200) algo)
;; (transduce-vector (list->vector
;;                    (range 200)) algo)
;; (transduce-rand 50 algo)
;; ;; (prep-file "test.txt")
;; (transduce-file "test.txt" algo)

(provide (all-defined-out))
