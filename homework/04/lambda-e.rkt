#lang racket

(require "e-ast.rkt")

; return a e:value/lambda (just a value)
(define (e:eval-exp e env)
  (cond
    [(e:value? e) e]
    [(e:lambda? e) (e:closure env e)]
    [(e:variable? e) (hash-ref env (e:variable-name e))]
    [(e:apply? e) (e:eval-apply e env)]
    [else 'todo]))

(define (e:eval-apply e env)
  (define f (e:apply-func e))
  (define args (e:apply-args e))
  (cond
    [(e:lambda? f) (e:eval-closure (e:closure env f) args env)]
    [(e:variable? f)
     (define name (e:variable-name f))
     (define ref (hash-ref env name))
     (cond
       ; builtin function
       [(procedure? ref) (e:value (ref args env))]
       [(e:lambda? ref) (e:eval-closure (e:closure env ref) args env)])]
    [(e:apply? f)
     (define res (e:eval-exp f env))
     (if (e:closure? res) (e:eval-closure res args env)
         (error (format "e:eval-apply: Expected a closure but got:\n ~a\n" res)))]
    [else error (format "e:eval-apply: Unexpected expr: ~a\n" f)]))

(define (e:eval-closure f args env)
  (define clos-env (e:closure-env f))
  (define lambda (e:closure-f f))
  (define params (e:lambda-params lambda))
  (define syms (map e:variable-name params))
  ; now we only care the first expr in body
  (define body (first (e:lambda-body lambda)))
  ; 1. eval args using outer env
  (define vals (e:eval-all args env))
  ; 2. update closure env with args
  (define syms-vals (zip syms vals))
  (define new-env (insert-pairs clos-env syms-vals))
  ; 3. eval closure body with clos-env
  (e:eval-exp body new-env))

;;------------------ Builtin ------------------------
(define (insert-pair pair table)
    (define pa (car pair))
    (define pb (cdr pair))
    (hash-set table pa pb))

(define (insert-pairs table pairs)
    (foldr insert-pair table pairs))

(define (e:builtin:+ args env)
  (e:eager-eval + args env))

(define (e:builtin:- args env)
  (e:eager-eval - args env))

(define (e:builtin:* args env)
  (e:eager-eval * args env))

(define (e:builtin:/ args env)
  (e:eager-eval / args env))

(define (e:builtin:not args env)
  (e:eager-eval not args env))

(define (e:builtin:and args env)
  (define proc (λ (arg) (e:eval-to-val arg env)))
  (andmap proc args))

(define (e:builtin:or args env)
  (define proc (λ (arg) (e:eval-to-val arg env)))
  (ormap proc args))

(define (e:builtin:void _args _env)
  (void))

(define (e:builtin-env)
  (insert-pairs (hash) e:builtins))

(define e:builtins
  (list (cons '+ e:builtin:+)
        (cons '- e:builtin:-)
        (cons '* e:builtin:*)
        (cons '/ e:builtin:/)
        (cons 'not e:builtin:not)
        (cons 'or e:builtin:or)
        (cons 'and e:builtin:and)
        (cons 'void e:builtin:void)))

;;-----------------------------------------------------

;;----------------- Untility ------------------------
(define (match-any v vals)
  (ormap (curry equal? v) vals))

(define (zip l r)
  (map cons l r))

(define (e:eval-all exps env)
  (define proc (λ (exp) (e:eval-exp exp env)))
  (map proc exps))

(define (e:eval-to-val exp env)
  (e:value-val (e:eval-exp exp env)))

(define (e:eval-all-to-val exps env)
  (define proc (λ (exp) (e:eval-to-val exp env)))
  (map proc exps))

(define (e:eager-eval f args env)
  (define vals (e:eval-all-to-val args env))
  (apply f vals))

;;---------------------------------------------------

;;------------------- Unit Tests ----------------------
(require rackunit)
(require racket/trace)
(require "e-parse-ast.rkt")

(define (eval datum)
  (e:value-val (e:eval-exp (parse-ast datum) (e:builtin-env))))

(define start-point (current-inexact-milliseconds))

(check-equal? (eval '(+ 1 2 3)) (+ 1 2 3))
(check-equal? (eval '(- 1 2 3)) (- 1 2 3))
(check-equal? (eval '(* 1 2 3)) (* 1 2 3))
(check-equal? (eval '(/ 1 2 3)) (/ 1 2 3))
(check-equal? (eval '(and #t #f 3)) (and #t #f 3))
(check-equal? (eval '(and #t #t 3)) (and #t #t 3))
(check-equal? (eval '(or  #f #t 3)) (or  #f #t 3))
(check-equal? (eval '(or  #f #f 3)) (or  #f #f 3))
(check-equal? (eval '(((λ (x) (λ () x)) 10))) 10)
(check-equal? (eval '(((λ (x) (λ (y) (+ x y))) 10) 20)) 30)

(define long-datum
  '(((((((λ (x) (λ (y) (λ (z) (λ (i) (λ (j) (λ (k) (+ x y z i j k)))))))1)2)3)4)5)6))
(define eq-expr
  (((((((λ (x) (λ (y) (λ (z) (λ (i) (λ (j) (λ (k) (+ x y z i j k)))))))1)2)3)4)5)6))
(check-equal? (eval long-datum) eq-expr)

(define end-point (current-inexact-milliseconds))

(printf "~ams\n" (- end-point start-point))
