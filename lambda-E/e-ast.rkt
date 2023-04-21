#lang racket

(struct e:value (v) #:transparent)
(struct e:variable (name) #:transparent)
(struct e:lambda (params body) #:transparent)
(struct e:apply (func args) #:transparent)

(define (e:expr? e)
  (or (e:value? e) (e:variable e) (e:lambda? e) (e:apply? e)))

; represent environment


