#lang racket

(require rackunit)
(define ns (make-base-namespace))

(define (run-bool b)
  (((eval b ns) #t) #f))

(define TRUE '(lambda (a) (lambda (b) a)))
(define FALSE '(lambda (a) (lambda (b) b)))

(define (OR a b) (list (list a TRUE) b))

(define (AND a b) (list (list a b) FALSE))

(define (NOT a) (list (list a FALSE) TRUE))
(define (EQ a b) (list (list a b) (NOT b)))

; Test
(check-equal?
 (run-bool
  (EQ TRUE (OR (AND FALSE TRUE) TRUE)))
 (equal? #t (or (and #f #t) #t)))


