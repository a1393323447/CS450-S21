#lang racket

;;------------------------------------------------
; This file provide some function to check syntax
; including λ-S λ-E λ-F
;;------------------------------------------------

(require rackunit)

(provide (all-defined-out))

;;------------------------------------------------
; List -> Boolean
; if symlst is a List of Symbol then return true
(define (symbols? symlst)
  (and
   (list? symlst)
   (andmap symbol? symlst)))
; Example
(check-true (symbols? '()))
(check-true (symbols? '(x y z)))
(check-false (symbols? '(1 x y)))
;;-------------------------------------------------

;;-------------------------------------------------
; Datum -> Boolean
; if a datum is syntactically valid then return true
(define (lambda? datum)
  (match datum
    [(list 'lambda params _body) (symbols? params)]
    [(list 'λ params _body) (symbols? params)]
    [_ #f]))
; Example
(check-true (lambda? '(lambda () 1)))
(check-true (lambda? '(lambda (x) x)))
(check-true (lambda? '(λ (x y) (+ x z))))
(check-false (lambda? '(λ (x 1) 1)))
;;-------------------------------------------------

;;-------------------------------------------------
; Datum -> List or Symbol
; Extract lambda params or body depend on arg part
(define (extract-lambda quoted-lambda #:part part)
  (match quoted-lambda
    [(list 'lambda params body)
     (match part
       ['params params]
       ['body body])]
    [(list 'λ params body)
     (match part
       ['params params]
       ['body body])]
    [_ (error "expected a lambda")]))
; ------------- Extract lambda params -------------
; Datum -> List
(define (lambda-params quoted-lambda)
  (extract-lambda quoted-lambda #:part 'params))
; Example
(check-equal? (lambda-params '(λ () x)) '())
(check-equal? (lambda-params '(λ (x y) x)) '(x y))
; ------------- Extract lambda body ---------------
; Datum -> List or Symbol
(define (lambda-body quoted-lambda)
  (extract-lambda quoted-lambda #:part 'body))
; Example
(check-equal? (lambda-body '(λ () x)) 'x)
(check-equal? (lambda-body '(λ (x y) (+ x y))) '(+ x y))
;;--------------------------------------------------

;;--------------------------------------------------
; Datum -> Boolean
; Returns a boolean whether or not the datum is a function application.
(define (apply? datum)
  (and
   ; all function apply must match `( ... )` which means datum must be a list
   (list? datum)
   (or
    ; if datum is list the first elem must be:
    ; - a symbol: (f x y)
    ; - a lambda: ((λ (x) x) 10)
    ; - a apply: (((λ (x) (λ () x) )10))
    ;             |        -------    |
    ;             |         lambda    |
    ;              -------------------
    ;           a apply that return a funtion
    (symbol? (first datum))
    (lambda? (first datum))
    (apply? (first datum)))))
; Example
(check-true (apply? '(x)))
(check-true (apply? '((λ () 1))))
(check-true (apply? '((f x y))))
(check-true (apply? '(((λ (x) (λ () x))10))))
(check-false (apply? '(1)))
(check-false (apply? '(#\x)))
;;---------------------------------------------------

;;---------------------------------------------------
(define (extract-apply ap #:part part)
  (if (apply? ap)
      (match part ['func (first ap)] ['args (rest ap)])
      (error "expected a function apply")))
; Datum -> Symbol or Lambda (which is a List)
(define (apply-func ap)
  (extract-apply ap #:part 'func))
; Example
(check-equal? (apply-func '(f)) 'f)
(check-equal? (apply-func '(f x y)) 'f)
(check-equal? (apply-func '(((λ () (λ (x y) x))) x y)) '((λ () (λ (x y) x))))
; Datum -> List
(define (apply-args ap)
  (extract-apply ap #:part 'args))
; Example
(check-equal? (apply-args '(f)) '())
(check-equal? (apply-args '(f x y)) '(x y))
(check-equal? (apply-args '((λ () f) x y)) '(x y))
;;---------------------------------------------------

;;---------------------------------------------------
; Datum -> Boolean
; return a boolean whether or not the datum is a literal
(define (literal? datum)
  (not(or (list? datum) (symbol? datum))))
; Example
(check-true (literal? '1))
(check-true (literal? '#\H))
(check-true (literal? '"1"))
(check-false (literal? 'x))
(check-false (literal? '(f x y)))
(check-false (literal? '(λ (x) x)))
;;---------------------------------------------------

;;---------------------------------------------------
; Datum -> Boolean
; return a boolean whether or not the datum is a basic define
(define (define-basic? datum)
  (match datum
    ; basically everything can be an expr on syntax check
    [(list 'define sym _expr) (symbol? sym)]
    [_ #f]))
; Example
(check-true (define-basic? '(define define 10)))
(check-true (define-basic? '(define x (f 1 2))))
(check-true (define-basic? '(define lambda 10)))
(check-true (define-basic? '(define x lambda)))
;;----------------------------------------------------

;;----------------------------------------------------
; Datum -> Boolean
; return a boolean whether or not the datum is a function define
(define (define-func? datum)
  (match datum
    [(list 'define name-params _body) (symbols? name-params)]
    [_ #f]))
; Example
(check-true (define-func? '(define (f) (+ x y))))
(check-true (define-func? '(define (f x y) (+ x y))))
(check-false (define-func? '(define (f 1 y) (+ x y))))
;;----------------------------------------------------

;;----------------------------------------------------
; Datum -> Boolean
; return a boolean whether or not the datum is a define
(define (define? datum)
  (or (define-basic? datum) (define-func? datum)))
(check-true (define? '(define (f) (+ x y))))
;;----------------------------------------------------
