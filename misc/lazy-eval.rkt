#lang racket

(require rackunit)

; Lazy evaluation
; - Using funcs to delay computation
; - Lazy evaluation as a form of controlling execaution
; - Lazy evaluation as data-structure
; - Functional patterns applied to delayed

; evaluation
; - eager
; - lazy

; Using funcs to delay computation
(define (if:eager b l r)
  (cond [b l]
        [else r]))

(define (if:lazy b l r)
  (cond [b (l)]
        [else (r)]))

(define (fact n)
  (if:lazy (= n 0)
    (thunk 1) ; same as (λ () 1)
    (thunk (* n (fact (- n 1))))))

(check-equal? (fact 10) 3628800)

; Promise
; Lazy evaluation as a form of controlling execaution
; Lazy evaluation as data-structure
(define heavy-work
  (thunk (display "busy...\n") (sleep 1) 10))

(display "----- without promise -----\n")
(check-equal? (heavy-work) 10)
(check-equal? (heavy-work) 10)
(check-equal? (heavy-work) 10)
(display "----- all checks pass -----\n\n")

(display "------ with promise -------\n")
(define heavy-work-prom (delay (heavy-work)))
(check-equal? (force heavy-work-prom) 10)
(check-equal? (force heavy-work-prom) 10)
(check-equal? (force heavy-work-prom) 10)
(display "----- all checks pass -----\n\n")

; Simple implement of promise
(struct promise (body run?))

(define (mk-promise th)
  (promise th #f))

(define (promise-sync p)
  (cond [(promise-run? p) p]
        [else
         (define th (promise-body p))
         (promise (th) #t)]))

(define (promise-get p)
  (cond [(promise-run? p) (promise-body p)]
        [else (error "Run (promise-sync) first!")]))

(define (promise-repeat n prom)
  (cond [(<= n 0) (void)]
        [else
         (promise-repeat (- n 1) (promise-sync prom))]))

(promise-repeat 3 (mk-promise (thunk (display "busy...\n") (sleep 1) 3)))


