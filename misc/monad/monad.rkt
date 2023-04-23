#lang racket

(require "env.rkt")

(require rackunit)

(define (run-state h op) (eff-state (op h)))
(define expected-heap
  (heap
   (hash (handle 0) 1
         (handle 1) 2
         (handle 2) 3)))

;;-------------------- Program 1 ------------------------
(define (prog1 h1)
  ; allocate x with 1
  (define eff-x (heap-alloc h1 1))
  (define x (eff-res eff-x))
  (define h2 (eff-state eff-x))
  ; allocate y with 2
  (define eff-y (heap-alloc h2 2))
  (define y (eff-res eff-y))
  (define h3 (eff-state eff-y))
  ; allocate z with (+ x y)
  (heap-alloc h3 (+ (heap-get h3 y) (heap-get h3 x))))

(check-equal? (run-state (empty-heap) prog1) expected-heap)
;;--------------------------------------------------------

;;-------------------- Program 2 -------------------------
;; We first encapsulate the difference in function `num` and `add`
;; so that we can see the repeated pattern
;;
;; Functions like `num` and `add`, takes a state and returns an effect
;; that pairs some state with some result, named as `effectful operation`
;;

(define (num n)
  ; takes a heap -> produces an eff
  (λ (h) (heap-alloc h n)))

(define (add x y)
  ; takes a heap -> produces an eff
  (λ (h)
    (define v (+ (heap-get h x) (heap-get h y)))
    (heap-alloc h v)))

(define (prog2 h1)
  ; takes a heap -> produces an eff
  
  ; allocate x with 1
  (define eff-x ((num 1) h1))
  (define x (eff-res eff-x))
  (define h2 (eff-state eff-x))
  ; allocate y with 2
  (define eff-y ((num 2) h2))
  (define y (eff-res eff-y))
  (define h3 (eff-state eff-y))
  ; allocate z with (+ x y)
  ((add x y) h3))

(check-equal? (run-state (empty-heap) prog2) expected-heap)

;;-------------------- Program 3 -------------------------
;; we always do something like:
;; 1. do a heap operation on the old hp and obtain a new hp
;; 2. pass the new hp to following code to perform more heap operations
;; so instead pass hp manually we can define a function to do the same thing

(define (bind heap-op cont)
  ; takes a heap -> produces an eff
  (lambda (hp)
    (define cur-eff (heap-op hp))
    (define res (eff-res cur-eff))
    (define new-hp (eff-state cur-eff))
    ((cont res) new-hp)))

; note that now prog3 just a basic define
(define (prog3)
  ; takes a heap -> produces an eff
  ; allocate x with 1
  (bind (num 1)
    (λ (x)
      ; allocate y with 2
      (bind (num 2)
        (λ (y)
          ; allocate z with (+ x y)
          (add x y))))))
(check-equal? (run-state (empty-heap) (prog3)) expected-heap)
;;--------------------------------------------------------

;;-------------------- Program 4 -------------------------
;; See stack-machines.rkt first
;; 
;; the `bind`s in our code are pretty annoying and ugly
;; we can improve our code with macro

(define-syntax do
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (λ (var) (do rest ...)))]
    ; No binding operator, just ignore the return value
    {(_ mexp rest ...)        (bind mexp (λ (_) (do rest ...)))}))

(define (prog4)
  (do
    x <- (num 1)
    y <- (num 2)
    (add x y)))
(check-equal? (run-state (empty-heap) (prog4)) expected-heap)

; if we want to do some pure operations and assign it to a variable in do context
; we need to use this helper function which sets the res to v
; and keeps the s remain unchanged
(define (pure v)
  (λ (s) (eff s v)))
;;--------------------------------------------------------

;;-------------------- Summary: the Monad -------------------------------------------
;; A monad is a functional pattern which can be categorized of two base combinators:
;; - Bind: combines two effectful operations O1 and O2. Operation O1 produces a value
;;   that consumed by operation O2
;; - Pure: converts a pure value to a monadic operation, which can then be chained
;;   with bind.
;;-----------------------------------------------------------------------------------
