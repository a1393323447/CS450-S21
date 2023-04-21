#lang racket

(require "../s-ast.rkt")

(define (s:eval-exp e)
  (cond
    [(s:void? e) (void)]
    [(s:bool? e) (s:bool-val e)]
    [(s:number? e) (s:number-val e)]
    [(s:variable? e) (s:variable-name e)]
    [(s:lambda? e) e]
    [(s:apply? e) (s:eval-apply e)]
    [else 'todo]))

(define (s:eval-apply e)
  (define f (s:eval-exp (s:apply-func e)))
  (define args (s:apply-args e))
  (cond
    [(s:builtin? f) ((s:get-builtin f) args)]
    [(s:lambda? f) (s:lambda-apply f args)]
    [else 'todo]))

(define (s:subst exp var val)
  (define (subst cur-exp)
    (cond
      [(s:value? cur-exp) cur-exp]
      [(s:variable? cur-exp) (if (equal? cur-exp var) val cur-exp)]
      [(s:lambda? cur-exp)
       (define params (s:lambda-params cur-exp))
       (define body (s:lambda-body cur-exp))
       (cond
         [(match-any var params) cur-exp]
         [else
          (define s-body (map subst body))
          (s:lambda params s-body)])]
      [(s:apply? cur-exp)
       (define f (s:apply-func cur-exp))
       (define args (s:apply-args cur-exp))
       (define sub-f (subst f))
       (define sub-args (map subst args))
       (s:apply sub-f sub-args)]
      [else error "s:subst: Unexpected exp type"]))
  (subst exp))

(define (s:lambda-apply f args)
  (define s-args (map s:to-value (s:eval-all args)))
  (define params (s:lambda-params f))
  ; now we only care about the first body
  (define body (first (s:lambda-body f)))
  (define params-args (zip params s-args))
  (define (proc param-arg e)
    (define param (car param-arg))
    (define arg (cdr param-arg))
    (s:subst e param arg))
  (define subst-body (foldr proc body params-args))
  (s:eval-exp subst-body))

(define s:builtins
  (list '+
        '-
        '*
        '/
        'and
        'or
        'not))

(define (s:builtin? f)
  (match-any f s:builtins))

(define (s:get-builtin name)
  (match name
    ['+ s:builtin:+]
    ['- s:builtin:-]
    ['* s:builtin:*]
    ['/ s:builtin:/]
    ['not s:builtin:not]
    ['or s:builtin:or]
    ['and s:builtin:and]
    [_ error "s:eval: Unknown builtin"]))

;;------------------ Builtin ------------------------
(define (s:builtin:+ args)
  (s:eager-eval + args))

(define (s:builtin:- args)
  (s:eager-eval - args))

(define (s:builtin:* args)
  (s:eager-eval * args))

(define (s:builtin:/ args)
  (s:eager-eval / args))

(define (s:builtin:not args)
  (s:eager-eval not args))

(define (s:builtin:and args)
  (andmap s:eval-exp args))

(define (s:builtin:or args)
  (ormap s:eval-exp args))
;;-----------------------------------------------------

;;----------------- Untility ------------------------
(define (match-any v vals)
  (ormap (curry equal? v) vals))

(define (zip l r)
  (map cons l r))

(define (s:eval-all exps)
  (map s:eval-exp exps))

(define (s:eager-eval f args)
  (apply f (s:eval-all args)))

;;---------------------------------------------------

;;------------------- Unit Tests ----------------------
(require rackunit)
(require "../02/parse-ast.rkt")

(define (eval datum)
  (s:eval-exp (parse-ast datum)))

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