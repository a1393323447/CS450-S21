#lang racket

(require rackunit)
(require "../syntax-check.rkt")
(require "../e-ast.rkt")

(provide parse-ast)

(define (to-list x)
  (if (list? x) x (list x)))

(define (parse-ast datum)
  (cond
    [(literal? datum) (e:value datum)]
    [(symbol? datum) (e:variable datum)]
    [(lambda? datum) (parse-lambda datum)]
    [(define-basic? datum) (parse-define-basic datum)]
    [(define-func? datum) (parse-define-func datum)]
    [(apply? datum) (parse-apply datum)]
    [else (error "unexpected datum")]))

(define (parse-lambda datum)
  (define-values (params body) (match datum
    [(list 'lambda params body) (values params body)]
    [(list 'λ params body) (values params body)]))
  (define s-params (map e:variable params))
  (define s-body (parse-ast body))
  (e:lambda s-params (to-list s-body)))

(define (parse-define-basic datum)
  (match datum
    [(list 'define name expr) (e:define name (parse-ast expr))]))

(define (parse-define-func datum)
  (match datum
    [(list 'define f-params body)
     (define name (first f-params))
     (define params (rest f-params))
     (define s-name (e:variable name))
     (define s-params (map e:variable params))
     (define s-body (parse-ast body))
     (define lambda (e:lambda s-params (to-list s-body)))
     (e:define s-name lambda)]))

(define (parse-apply datum)
  (define f (first datum))
     (define args (rest datum))
     (define sf (parse-ast f))
     (define sargs (map (λ (n) (parse-ast n)) args))
     (e:apply sf sargs))

(check-equal? (parse-ast 'x) (e:variable 'x))
(check-equal? (parse-ast '10) (e:value 10))

(check-equal?
 (parse-ast '(lambda (x) x))
 (e:lambda (list (e:variable 'x)) (list (e:variable 'x))))

(check-equal?
 (parse-ast '(define (f y) (+ y 10)))
 (e:define
  (e:variable 'f)
  (e:lambda
   (list (e:variable 'y))
   (list (e:apply (e:variable '+) (list (e:variable 'y) (e:value 10)))))))

