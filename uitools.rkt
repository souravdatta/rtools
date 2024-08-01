#lang racket

(require racket/gui)

(define timer-frame%
  (class frame%
    (super-new)
    (field [frm-timer #f])
    (init-field [interval 1000])
    (define/public (set-timer fn)
      (when (not frm-timer)
        (set! frm-timer (new timer%
                         [notify-callback fn]
                         [interval interval]))))
    (define/public (stop-timer)
      (when frm-timer
        (send frm-timer stop)))
    (define/public (start-timer)
      (when frm-timer
        (send frm-timer start interval)))
    (define (on-close)
      (when frm-timer
        (send frm-timer stop)))
    (augment on-close)))


(define (timer #:secs [secs 0] #:mins [mins 0] #:hrs [hrs 0])
  (define total-secs (+ (* hrs 3600)
                        (* mins 60)
                        secs))
  (define f (new timer-frame%
                 [label (format "Timer ~aH ~aM ~aS" hrs mins secs)]
                 [width 100]
                 [height 60]))
  (define vpane (new vertical-pane%
                     [parent f]
                     [horiz-margin 10]))
  (define lbl (new message%
                   [parent vpane]
                   [min-width 80]
                   [label (format "Starting countdown for ~a s" total-secs)]))
  (define prgs (new gauge%
                    [parent vpane]
                    [label ""]
                    [range total-secs]))
  (send f set-timer (thunk
                     (if (>= total-secs 0)
                         (begin
                           (send lbl set-label
                                 (format "Remaining seconds: ~a" total-secs))
                           (send prgs set-value total-secs)
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
  (define hpane (new horizontal-pane%
                     [parent vpane]))
  (define btn-start (new button%
                         [parent hpane]
                         [label "Start"]
                         [callback (λ (b e)
                                     (send f start-timer))]))
  (define btn-stop (new button%
                        [parent hpane]
                        [label "Stop"]
                        [callback (λ (b e)
                                    (send f stop-timer))]))
  (send f set-timer
        (thunk
         (send lbl set-label
               (format "Elapsed seconds: ~a" total-secs))
         (set! total-secs (+ total-secs 1))))
  (send f stop-timer)
  (send f show #t)
  f)

(provide (all-defined-out))
