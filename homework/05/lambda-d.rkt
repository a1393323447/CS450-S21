#lang racket

(require "d-ast.rkt")
(require "env.rkt")

; (-> expr? handle? heap? eff?)
(define (d:eval-exp exp env hp)
  (cond
    [(d:val? exp) (eff hp exp)]
    [(d:variable? exp)
     (define sym (d:variable-name exp))
     (define val (environ-get hp env sym))
     (eff hp val)]
    [(d:lambda? exp) (eff hp (d:closure env exp))]
    [(d:define? exp) (d:eval-define exp env hp)]
    [(d:apply? exp) (d:eval-apply exp env hp)]
    [else (error (format "d:eval-exp: Unexpcted exp: ~a\n" exp))]))

(define (d:eval-define exp env hp)
  ; 1. get the define symbol and expr
  (define var (d:variable-name (d:define-name exp)))
  (define expr (d:define-expr exp))
  ; 2. eval expr with cur hp and env
  ; and get the new hp and val(the res of expr)
  ;
  ; name the new heap as res-hp
  ; note that the res-hp may contains some new definitions
  ; which are introduced by the expr
  (define-values (res-hp val) (eff-destruct (d:eval-exp expr env hp)))
  ; 3. put the definition to cur env in res-hp and get the new-hp
  (define new-hp (environ-put res-hp env var val))
  ; 4. finally we return the effect. the return val of a define is void
  (eff new-hp (d:void)))

(define (d:eval-apply exp env hp)
  (define-values (ef args) (d:apply-destruct exp))
  (define-values (new-hp fn) (eff-destruct (d:eval-exp ef env hp)))
  (cond
    ; if fn is a procedure then it is a builtin function
    [(procedure? fn) (fn args env new-hp)]
    [(d:closure? fn)
     (define-values (Ef vars body) (d:closure-destruct fn))
     (define syms (map d:variable-name vars))
     (define vals (eff-res (d:eval-all args env new-hp)))
     (define syms-vals (zip syms vals))
     (define-values (exe-hp Eb)
       (eff-destruct (environ-push-pairs new-hp Ef syms-vals)))
     (define-values (res-hp return-vals) (eff-destruct (d:eval-all body Eb exe-hp)))
     (define return-val (last return-vals))
     (eff res-hp return-val)]))

;;------------------ Builtin ------------------------
(define (d:builtin:+ args env hp)
  (d:eager-eval + args env hp))

(define (d:builtin:- args env hp)
  (d:eager-eval - args env hp))

(define (d:builtin:* args env hp)
  (d:eager-eval * args env hp))

(define (d:builtin:/ args env hp)
  (d:eager-eval / args env hp))

(define (d:builtin:< args env hp)
  (d:eager-eval < args env hp))

(define (d:builtin:= args env hp)
  (d:eager-eval = args env hp))

(define (d:builtin:> args env hp)
  (d:eager-eval > args env hp))

(define (d:builtin:not args env hp)
  (d:eager-eval not args env hp))

(define (d:builtin:and args env hp)
  (cond
    [(empty? args) (eff hp (d:value #t))]
    [else
     (define arg (first args))
     (define-values (new-hp val) (eff-destruct (d:eval-exp arg env hp)))
     (cond [(not (d:to-rkt-val val)) (eff hp val)]
           [(empty? (rest args)) (eff new-hp val)]
           [else (d:builtin:and (rest args) env new-hp)])]))

(define (d:builtin:or args env hp)
  (cond
    [(empty? args) (eff hp (d:value #f))]
    [else
     (define arg (first args))
     (define-values (new-hp val) (eff-destruct (d:eval-exp arg env hp)))
     (cond [(d:to-rkt-val val) (eff hp val)]
           [(empty? (rest args)) (eff new-hp val)]
           [else (d:builtin:or (rest args) env new-hp)])]))

(define (d:builtin-env)
  (define root-locals (insert-pairs (hash) d:builtins))
  (define root-frm (root-frame root-locals))
  (environ-init root-frm))

(define d:builtins
  (list (cons '+ d:builtin:+)
        (cons '- d:builtin:-)
        (cons '* d:builtin:*)
        (cons '/ d:builtin:/)
        (cons '< d:builtin:<)
        (cons '= d:builtin:=)
        (cons '> d:builtin:>)
        (cons 'not d:builtin:not)
        (cons 'or d:builtin:or)
        (cons 'and d:builtin:and)))

(define (d:builtin? sym)
  (match-any sym (map car (d:builtins))))

;;-----------------------------------------------------

;;----------------- Untility ------------------------
(define (match-any v vals)
  (ormap (curry equal? v) vals))

(define (zip l r)
  (map cons l r))

(define (insert-pair pair table)
    (define pa (car pair))
    (define pb (cdr pair))
    (hash-set table pa pb))

(define (insert-pairs table pairs)
    (foldr insert-pair table pairs))

; return (eff new-hp (map-f vals))
(define (d:map-eval map-f exps env hp)
  (define res-hp hp)
  (define (eval-iter exprs cur-hp)
    (cond
      [(empty? exprs) (begin (set! res-hp cur-hp) empty)]
      [else
       (define exp (first exprs))
       (define-values (new-hp val) (eff-destruct (d:eval-exp exp env cur-hp)))
       (cons (map-f val) (eval-iter (rest exprs) new-hp))]))
  (eff res-hp (eval-iter exps hp)))

(define (d:eval-all exps env hp)
  (d:map-eval identity exps env hp))

(define (d:eval-all-to-rkt-val exps env hp)
  (d:map-eval d:to-rkt-val exps env hp))

(define (d:eager-eval f args env hp)
  (define-values (new-hp vals) (eff-destruct (d:eval-all-to-rkt-val args env hp)))
  (define res (d:to-val (apply f vals)))
  (eff new-hp res))

;;---------------------------------------------------
(require "d-parse-ast.rkt")
(require rackunit)

(define (eval datum)
  (define hp-env (d:builtin-env))
  (define hp (eff-state hp-env))
  (define env (eff-res hp-env))
  (d:to-rkt-val (eff-res (d:eval-exp (parse-ast datum) env hp))))

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
(check-equal? (eval '((and #t (λ () (define f 10) f))))
                     ((and #t (λ () (define f 10) f))))

(define long-datum
  '(((((((λ (x) (λ (y) (λ (z) (λ (i) (λ (j) (λ (k) (+ x y z i j k)))))))1)2)3)4)5)6))
(define eq-expr
  (((((((λ (x) (λ (y) (λ (z) (λ (i) (λ (j) (λ (k) (+ x y z i j k)))))))1)2)3)4)5)6))
(check-equal? (eval long-datum) eq-expr)

(define end-point (current-inexact-milliseconds))

(printf "~ams\n" (- end-point start-point))
