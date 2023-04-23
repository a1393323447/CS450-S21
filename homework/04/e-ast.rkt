#lang racket

(provide (all-defined-out))

(struct e:value (val) #:transparent)
(struct e:variable (name) #:transparent)
(struct e:lambda (params body) #:transparent)
(struct e:define (name expr) #:transparent)
(struct e:apply (func args) #:transparent)

; Run time value
(struct e:closure (env f) #:transparent)
