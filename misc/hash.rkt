#lang racket

(require rackunit)

; create hash table
(define h (hash))
(check-true (hash? h))
(check-equal? (hash-count h) 0)


(define h1 (hash-set h "foo" 20))
(check-equal? (hash-ref h1 "foo") 20)

(define h2 (hash-set h1 "foo" 30))
(check-equal? (hash-ref h2 "foo") 30)


