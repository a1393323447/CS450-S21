#lang racket

(require rackunit)

; (define (concat-nums:v1 nums)
;   (define (concat-iter acc l)
;     (cond
;       [(empty? l) acc]
;       [else (concat-iter
;              (string-append acc " " (number->string (first l)))
;              (rest l))]))
;   (concat-iter ">" nums))

; intersperse is similar to concat-nums
; basically intersperse just insert a 0 between ele and acc-list
(define (intersperse:v1 nums)
  (define (intersperse-iter acc l)
    (cond
      [(empty? l) acc]
      [else (intersperse-iter
             (string-append acc " 0 " (number->string (first l)))
             (rest l))]))
  (intersperse-iter "" nums))
(check-equal? (intersperse:v1 '(1 2 3)) " 0 1 0 2 0 3")
; but we notice that
; - we should do something like foldr
; - we should remove the tailing 0 that means we only adding zeros when
;   (rest lst) not empty

(define (foldr step base-case l)
  (define (foldr-iter accum l)
    (cond
      [(empty? l) (accum base-case)]
      [else (foldr-iter
             (λ (new-lst) (accum (step (first l) new-lst)))
             (rest l))]))
  (foldr-iter identity l))

(define (intersperse l v)
  (define (step x acc)
    (cond
      [(empty? acc) (cons x acc)]
      [else (cons x (cons v acc))]))
  (foldr step empty l))
; Unit tests
(check-equal? (intersperse (list 1 2 3) 0) (list 1 0 2 0 3))
(check-equal? (intersperse (list 1 2 3) 1) (list 1 1 2 1 3))
(check-equal? (intersperse (list) 0) (list))
(check-equal? (intersperse (list 1) 1) (list 1))
