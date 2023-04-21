#lang racket

(require rackunit)
(require "../syntax-check.rkt")
(require "../s-ast.rkt")

(provide parse-ast)

(define (to-list x)
  (if (list? x) x (list x)))

(define (parse-ast datum)
  (cond
    [(number? datum) (s:number datum)]
    [(boolean? datum) (s:bool datum)]
    [(symbol? datum) (s:variable datum)]
    [(lambda? datum) (parse-lambda datum)]
    [(define-basic? datum) (parse-define-basic datum)]
    [(define-func? datum) (parse-define-func datum)]
    [(apply? datum) (parse-apply datum)]
    [else (error "unexpected datum")]))

(define (parse-lambda datum)
  (define-values (params body) (match datum
    [(list 'lambda params body) (values params body)]
    [(list 'λ params body) (values params body)]))
  (define s-params (map s:variable params))
  (define s-body (parse-ast body))
  (s:lambda s-params (to-list s-body)))

(define (parse-define-basic datum)
  (match datum
    [(list 'define name expr) (s:define name (parse-ast expr))]))

(define (parse-define-func datum)
  (match datum
    [(list 'define f-params body)
     (define name (first f-params))
     (define params (rest f-params))
     (define s-name (s:variable name))
     (define s-params (map s:variable params))
     (define s-body (parse-ast body))
     (define lambda (s:lambda s-params (to-list s-body)))
     (s:define s-name lambda)]))

(define (parse-apply datum)
  (cond
    [(equal? datum '(void)) (s:void)]
    [else
     (define f (first datum))
     (define args (rest datum))
     (define sf (parse-ast f))
     (define sargs (map (λ (n) (parse-ast n)) args))
     (s:apply sf sargs)]))

(check-equal? (parse-ast 'x) (s:variable 'x))
(check-equal? (parse-ast '10) (s:number 10))

(check-equal?
 (parse-ast '(lambda (x) x))
 (s:lambda (list (s:variable 'x)) (list (s:variable 'x))))

(check-equal?
 (parse-ast '(define (f y) (+ y 10)))
 (s:define
  (s:variable 'f)
  (s:lambda
   (list (s:variable 'y))
   (list (s:apply (s:variable '+) (list (s:variable 'y) (s:number 10)))))))

