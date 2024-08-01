#lang racket

(require racket/gui)

(define timer-frame%
  (class frame%
    (super-new)
    (field [timer #f])
    (init-field [interval 1000])
    (define/public (set-timer fn)
      (when (not timer)
        (set! timer (new timer%
                         [notify-callback fn]
                         [interval interval]))))
    (define/public (stop-timer)
      (send timer stop))
    (define (on-close)
      (when timer
        (send timer stop)))
    (augment on-close)))


(define (timer #:secs [secs 0] #:mins [mins 0] #:hrs [hrs 0])
  (define total-secs (+ (* hrs 3600)
                        (* mins 60)
                        secs))
  (define f (new timer-frame%
                 [label (format "Timer ~a:~a:~a" hrs mins secs)]
                 [width 100]
                 [height 60]))
  (define vpane (new vertical-pane%
                     [parent f]
                     [horiz-margin 10]))
  (define lbl (new message%
                   [parent vpane]
                   [min-width 80]
                   [label (format "Starting countdown for ~a s" total-secs)]))
  (send f set-timer (thunk
                     (if (>= total-secs 0)
                         (begin
                           (send lbl set-label
                                 (format "Remaining seconds: ~a" total-secs))
                           (set! total-secs (- total-secs 1)))
                         (send f stop-timer))))
  (send f show #t)
  f)


(define (stopwatch)
  (define total-secs 0)
  (define f (new timer-frame%
                 [label (format "Stopwatch")]
                 [width 180]
                 [height 60]))
  (define vpane (new vertical-pane%
                     [parent f]
                     [horiz-margin 10]))
  (define lbl (new message%
                   [parent vpane]
                   [min-width 160]
                   [label ""]))
  (define btn (new button%
                   [parent vpane]
                   [label "Start"]
                   [callback (λ (b e)
                               (send f set-timer
                                     (thunk
                                      (send lbl set-label
                                            (format "Elapsed seconds: ~a" total-secs))
                                      (set! total-secs (+ total-secs 1))))
                               (send b enable #f))]))
  
  (send f show #t)
  f)

(provide (all-defined-out))
