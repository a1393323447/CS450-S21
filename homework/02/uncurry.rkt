#lang racket

(require rackunit)

(define (uncurry curried-fn)
  (λ (args-list)
    (foldl
     (λ (arg fn) (fn arg))
     curried-fn
     args-list)))

; Unit test
(define (f x y z w)
  (+ x y z w))
(define g (uncurry (curry f)))
(check-equal? 10 (g (list 1 2 3 4)))

(define (h x y z w)
  (- x y z w))
(define p (uncurry (curry h)))
(check-equal? (p (list 1 2 3 4)) (- 1 2 3 4))
