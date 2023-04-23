#lang racket

(provide (all-defined-out))

(struct s:void () #:transparent)
(struct s:bool (val) #:transparent)
(struct s:number (val) #:transparent)
(struct s:variable (name) #:transparent)
(struct s:lambda (params body) #:transparent)
(struct s:define (name expr) #:transparent)
(struct s:apply (func args) #:transparent)

(define (s:value? e)
  (or (s:void? e)
      (s:bool? e)
      (s:number? e)))

(define (s:to-value val)
  (cond
    [(void? val) (s:void)]
    [(boolean? val) (s:bool val)]
    [(number? val) (s:number val)]
    [else error "s:to-value: Unexpected value type"]))
