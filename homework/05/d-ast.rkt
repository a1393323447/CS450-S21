#lang racket

(provide (all-defined-out))

(struct d:void () #:transparent)
(struct d:value (val) #:transparent)
(struct d:variable (name) #:transparent)
(struct d:lambda (params body) #:transparent)
(struct d:define (name expr) #:transparent)
(struct d:apply (func args) #:transparent)

; Run time value
(struct d:closure (env f) #:transparent)

(define (d:val? v)
 (or (d:void? v)
     (d:value? v)
     (d:closure? v)))

(define (d:to-rkt-val v)
  (cond [(d:val? v) (if (d:value? v) (d:value-val v) v)]
        [else (error (format "d:to-val: Expected a d:val? but got ~a" v))]))

(define (d:to-val v)
  (cond [(d:val? v) v] [else (d:value v)]))

(define (d:lambda-destruct lam)
  (values (d:lambda-params lam)
          (d:lambda-body lam)))

(define (d:define-destruct def)
  (values (d:define-name def)
          (d:define-expr def)))

(define (d:apply-destruct app)
  (values (d:apply-func app)
          (d:apply-args app)))

(define (d:closure-destruct clos)
  (define f (d:closure-f clos))
  (define-values (params body) (d:lambda-destruct f))
  (values (d:closure-env clos)
          params
          body))
