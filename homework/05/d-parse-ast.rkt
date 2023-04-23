#lang racket

(require rackunit)
(require "../syntax-check.rkt")
(require "d-ast.rkt")

(provide parse-ast)

(define (to-list x)
  (if (list? x) x (list x)))

(define (parse-ast datum)
  (cond
    [(literal? datum) (d:value datum)]
    [(symbol? datum) (d:variable datum)]
    [(lambda? datum) (parse-lambda datum)]
    [(define-basic? datum) (parse-define-basic datum)]
    [(define-func? datum) (parse-define-func datum)]
    [(apply? datum) (parse-apply datum)]
    [else (error (format "parse-ast: Unexpected datum: ~a\n" datum))]))

(define (parse-lambda datum)
  (define-values (params body) (match datum
    [(list 'lambda params body ...) (values params body)]
    [(list 'λ params body ...) (values params body)]))
  (define s-params (map d:variable params))
  (define s-body (map parse-ast body))
  (d:lambda s-params s-body))

(define (parse-define-basic datum)
  (match datum
    [(list 'define name expr) (d:define (d:variable name) (parse-ast expr))]))

(define (parse-define-func datum)
  (match datum
    [(list 'define f-params body ...)
     (define name (first f-params))
     (define params (rest f-params))
     (define s-name (d:variable name))
     (define s-params (map d:variable params))
     (define s-body (map parse-ast body))
     (define lambda (d:lambda s-params s-body))
     (d:define s-name lambda)]))

(define (parse-apply datum)
  (cond
    [(equal? datum '(void)) (d:void)]
    [else
     (define f (first datum))
     (define args (rest datum))
     (define sf (parse-ast f))
     (define sargs (map (λ (n) (parse-ast n)) args))
     (d:apply sf sargs)]))

(check-equal? (parse-ast 'x) (d:variable 'x))
(check-equal? (parse-ast '10) (d:value 10))

(check-equal?
 (parse-ast '(lambda (x) x))
 (d:lambda (list (d:variable 'x)) (list (d:variable 'x))))

(check-equal?
 (parse-ast '(define (f y) (+ y 10)))
 (d:define
  (d:variable 'f)
  (d:lambda
   (list (d:variable 'y))
   (list (d:apply (d:variable '+) (list (d:variable 'y) (d:value 10)))))))

