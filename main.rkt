#lang racket


(require rackunit)

(require "utils.rkt")


(test-begin
 (let ([h (make-hasheq (list
                        (cons 'a (make-hasheq (list
                                               (cons 'apple
                                                     (make-hasheq (list
                                                                   (cons 'fruit "tasty")
                                                                   (cons 'company "stylish")
                                                                   (cons 'computer "unstable"))))
                                               (cons 'album
                                                     (make-hasheq (list
                                                                   (cons 'name "Nasha")
                                                                   (cons 'singer "Blues")
                                                                   (cons 'genre "Jazz")))))))))])
   (check-equal? (chain-get h '(a apple fruit)) "tasty")
   (check-equal? (chain-get h '(a apple company)) "stylish")
   (check-equal? (chain-get h '(a apple computer)) "unstable")
   (check-equal? (chain-get h '(a album fruit)) #f)
   (check-equal? (chain-get h '(a album singer)) "Blues")
   (check-equal? (chain-get h '(a apple fruit album)) #f)
   (check-equal? (chain-get h '(a apple fruit name) #:default "None") "None")))
