#lang racket

; manually curry
(define (freeze f arg1)
  (define (get-arg2 arg2)
    (f arg1 arg2))
  get-arg2)

(define (frozen-*:v1 arg1)
  (freeze * arg1))
(define (frozen-+:v1 arg1)
  (freeze + arg1))

(define fr-*10:v1 (frozen-*:v1 10))
(define fr-+10:v1 (frozen-+:v1 10))

; use curry
(define frozen-*:v2 (curry *))
(define frozen-+:v2 (curry +))

(define fr-*10:v2 (frozen-*:v2 10))
(define fr-+10:v2 (frozen-+:v2 10))

; apply a list of args to a curry function
(define (apply-curry curry-fn arg-lst)
  (cond
    [(empty? arg-lst) curry-fn]
    [else (apply-curry (curry-fn (first arg-lst)) (rest arg-lst))]))

(define (add3 x y z) (+ x y z))
(define curry-add3 (curry add3))

; checks
(require rackunit)

(check-equal? 100 (fr-*10:v1 10))
(check-equal?  20 (fr-+10:v1 10))

(check-equal? 100 (fr-*10:v2 10))
(check-equal?  20 (fr-+10:v2 10))

(check-equal? 111 (apply-curry curry-add3 (list 1 10 100)))


