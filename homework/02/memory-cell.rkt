#lang racket

(require rackunit)

(define (rw-cell init)
  (lambda (lst)
    (match lst
      [(list) init]
      [(list v) (rw-cell v)]
      [_ (error "rw-cell: expected a list which has zero or one elem")])))

(define (ro-cell init)
  (lambda (lst)
    (match lst
      [(list) init]
      [(list _) (rw-cell init)]
      [_ (error "ro-cell: expected a list which has zero or one elem")])))

(define (cell-get cell) (cell (list)))
(define (cell-set cell x) (cell (list x)))

;;------------------ Unit tests ---------------------
(define w1 (rw-cell 10))
(check-equal? 10 (cell-get w1))
(define w2 (cell-set w1 20))
(check-equal? 20 (cell-get w2))

(define r1 (ro-cell 10))
(check-equal? 10 (cell-get r1))
(define r2 (cell-set r1 20))
(check-equal? 10 (cell-get r2))
