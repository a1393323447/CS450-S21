#lang racket

; In error monad, we have two continuation:
; - ok: we pass result x it to ok continue running
; - err: we pass result x it to err to handle the error
;
; so each operations would accept `ok` an `err` cont and call cont on demand

; pure is same as return
(define (pure x)
  (λ (ok err) (ok x)))

; return a value is always success
; so we call ok with x
(define (return x)
  (λ (ok err) (ok x)))

; to raise a error with value x
; we call err cont with value x
(define (raise x)
  (λ (ok err) (err x)))

; we can write a safe div like:
(define (safe-/ x y)
  (lambda (ok err)
    (cond [(= 0 y) (err 'division-by-zero)]
          [else (ok (/ x y))])))

; we use bind to chain two operations
; To chain two operations, we need to call op2 in op1's ok continuation
; so that when op1 success, the op2 would be called with op1's res
(define (bind op1 op2)
  (λ (ok err)
    ; if op1 success, the op1 would call its ok continuation
    ; in this case, op1's ok continuation is ok-cont
    (define (ok-cont res)
      ; in this ok cont, we would call op2 with op1's res
      ((op2 res) ok err))
    ; here we pass ok-cont to op1's ok
    (op1 ok-cont err)))

(define-syntax do
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (lambda (var) (do rest ...)))]
    ; No binding operator, just ignore the return value
    [(_ mexp rest ...)        (bind mexp (lambda (_) (do rest ...)))]))

; now we define the basic ok/err handler
(define (print-ok x)
  (printf "print-ok: ~a\n" x))

(define (print-err x)
  (printf "print-err: ~a\n" x))

(define (run-cps o)
  (o print-ok print-err))

(define prog1
  (do
    x <- (safe-/ 10 2)
    (raise 'oh-no)
    y <- (return (displayln "can we reach here?"))
    (return (+ x 10))))
(run-cps prog1)

(define prog2
  (do
    x <- (safe-/ 10 2)
    y <- (safe-/ 14 7)
    (return (+ x y))))
(run-cps prog2)

(define prog3
  (do
    x <- (safe-/ 10 2)
    y <- (safe-/ 14 7)
    (pure (printf "pure: ~a\n" (+ x y)))
    (return (+ x y))))
(run-cps prog3)
