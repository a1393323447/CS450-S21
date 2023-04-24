#lang racket

(struct return-ok (value) #:transparent)
(struct return-error (value) #:transparent)

(define (cps-bind o1 o2)
  (lambda (ret err)
    (o1 (lambda (res) ((o2 res) ret err)) err)))

(define (pure x)
  (lambda (ret err)
    (ret x)))

(define (run-cps op)
  (op return-ok return-error))

(define-syntax do
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (cps-bind mexp (lambda (var) (do rest ...)))]
    ; No binding operator, just ignore the return value
    [(_ mexp rest ...)        (cps-bind mexp (lambda (_) (do rest ...)))]))

; A suspended compuation holds things
; - value: the value being yielded
; - ok: the ok-contimuation that we must use to resume
(struct suspend (value ok) #:transparent)

; A yield does not use `ok` nor `err` to return its result
; Instead, it just a special value called `susp`
(define (yield v)
  (λ (ok err) (suspend v ok)))

; Resume simply calls the continuation and passes void to it
;
; Imagine you have some code:
;     y <- (yield x) ; y is always void
;     ... ; k
; 
; Resume takes the continuation (λ (y) k) and passes void to it.
(define (resume s)
  ((suspend-ok s) (void)))

; Now that we have resume, what we can do with it is as follows:
;
; For each value yield point `x` in computation `m`, call `(f x)`.
; for-each is a cps-operation that returns whatever `m` returns
(define (for-each m f)
  (λ (ok err)
    (define (each res)
      (cond
        [(suspend? res)
         ; we found a: (yield x)
         ; call f one the suspended value
         ; (f x)
         (f (suspend-value res))
         ; continue running the suspended task with `resume`
         (each (resume res))]
        [else (ok res)]))
    (each (m (λ (x) x) err))))

; build a list with all the yield points of `m` and return that list.
(define (to-list m)
  (λ (ok err)
    (define (each res acc)
      (cond
        [(suspend? res)
         (each
          ; continue running the suspended task
          (resume res)
          ; we continue buildingour accumulated values
          (cons (suspend-value res) acc))]
        [else (ok (reverse acc))]))
    (each (m (λ (x) x) err) empty)))

(define three-values
  (do
    (yield 1)
    (yield 2)
    (yield 3)
    (yield 10)
    str <- (pure "hello world!")
    (yield str)
    (pure "success")))

(run-cps
  (for-each three-values
    (lambda (x) (printf "Got element: ~a\n" x))))

(displayln "-----------------------------------------")

(run-cps
  (do
    l <- (to-list three-values)
    (pure (printf "-> Got a list: ~a\n" l))
    (pure (cons 0 l))))
