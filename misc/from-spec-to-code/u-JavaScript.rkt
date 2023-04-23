#lang racket

;;--------------------- AST --------------------
(struct j:number (val) #:transparent)
(struct j:variable (name) #:transparent)
(struct j:sub (e1 e2) #:transparent)
(struct j:assign (var exp) #:transparent)
(struct j:while (cond body) #:transparent)
(struct j:log (exp) #:transparent)
(struct j:seq (cur rest) #:transparent)

(define (j:program? p)
  (or (j:assign? p)
      (j:while? p)
      (j:log? p)
      (j:seq? p)))

(define (j:expr? exp)
  (or (j:number? exp)
      (j:variable? exp)
      (j:sub? exp)))

;;----------------------------------------------

;;------------------- Eval ---------------------
(define/contract (j:eval vars prog)
  (-> hash? j:program? hash?)
  
  (cond
    [(j:assign? prog)
     (define v (j:eval-exp vars (j:assign-exp prog)))
     (hash-set vars (j:variable-name (j:assign-var prog)) v)]
    [(j:log? prog)
     (define v (j:eval-exp vars (j:log-exp prog)))
     (displayln v)
     vars]
    [(j:seq? prog)
     (define p1 (j:seq-cur prog))
     (define p2 (j:seq-rest prog))
     (define m2 (j:eval vars p1))
     (define m3 (j:eval m2 p2))
     m3]
    [(j:while? prog)
     (define v (j:eval-exp vars (j:while-cond prog)))
     (cond [(equal? v 0) vars]
           [else
            (define p (j:while-body prog))
            (define m1 (j:eval vars (j:seq p prog)))
            m1 ])]))

(define/contract (j:eval-exp vars exp)
  (-> hash? j:expr? number?)
  (cond
    [(j:number? exp) (j:number-val exp)]
    [(j:variable? exp) (hash-ref vars (j:variable-name exp))]
    [(j:sub? exp)
     (define val-1 (j:eval-exp vars (j:sub-e1 exp)))
     (define val-2 (j:eval-exp vars (j:sub-e2 exp)))
     (- val-1 val-2)]))
;;----------------------------------------------

(define x (j:variable 'x))
(define n (j:number 10))
(define one (j:number 1))

(define prog
  (j:seq
   (j:assign x n)
   (j:while x
            (j:seq
             (j:log x)
             (j:assign x (j:sub x one))))))

(j:eval (hash) prog)

