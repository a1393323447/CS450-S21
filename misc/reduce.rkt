#lang racket

;;----------------------- flodr ---------------------------
; Demonstrate how to abstract out a commond pattern
; from the following three functions
;; --------------------------------------------------------
(define (map:v1 f l)
  (cond
    [(empty? l) empty]
    [else (cons (f (first l)) (map:v1 f (rest l)))]))

(define (append:v1 l r)
  (cond
    [(empty? l) r]
    [else (cons (first l) (append:v1 (rest l) r))]))

(define (filter:v1 to-keep? l)
  (cond
    [(empty? l) empty]
    [(to-keep? (first l)) (cons (first l) (filter:v1 to-keep? (rest l)))]
    [else (filter:v1 to-keep? (rest l))]))

; Generalize 
; (define (rec step base-case l)
;   (cond
;     [(empty? l) base-case]
;     [else (step (first l) (rec (rest l)))]))
;                  |  |  |
;                  V  V  V
; ps: ntc stands for non-tail recursive
(define (foldr:ntc step base-case l)
  (cond
    [(empty? l) base-case]
    [else (step (first l) (foldr:ntc step base-case l))]))
; Optimize
(define (foldr step base-case l)
  (define (foldr-iter accum l)
    (cond
      [(empty? l) (accum base-case)]
      [else (foldr-iter
             (λ (new-lst) (accum (step (first l) new-lst)))
             (rest l))]))
  (foldr-iter identity l))
; Use foldr to define map filter and append
; Because foldr is tail recursive, and
; map, filter, append are builded upon foldr.
; Now they are all tail recursive
(define (map:v2 f l)
  (define (step x acc)
    (cons (f x) acc))
  (foldr step empty l))

(define (filter:v2 to-keep? l)
  (define (step x acc)
    (cond
      [(to-keep? x) (cons x acc)]
      [else acc]))
  (foldr step empty l))

(define (append:v2 l r)
  (foldr cons r l))

(require rackunit)

(check-equal?
 (map:v1 (λ (x) (* x 2)) '(1 2 3))
 '(2 4 6))
(check-equal?
 (map:v1 (λ (x) (+ x 1)) '(1 2 3))
 '(2 3 4))

(check-equal?
 (map:v2 (λ (x) (* x 2)) '(1 2 3))
 '(2 4 6))
(check-equal?
 (map:v2 (λ (x) (+ x 1)) '(1 2 3))
 '(2 3 4))

(check-equal?
 (append:v1 (list) (list 1 2 3 4))
 (list 1 2 3 4))
(check-equal?
 (append:v1 (list 1) (list 2 3 4))
 (list 1 2 3 4))
(check-equal?
 (append:v1 (list 1 2) (list 3 4))
 (list 1 2 3 4))
(check-equal?
 (append:v1 (list 1 2 3) (list 4))
 (list 1 2 3 4))
(check-equal?
 (append:v1 (list 1 2 3 4) (list))
 (list 1 2 3 4))

(check-equal?
 (append:v2 (list) (list 1 2 3 4))
 (list 1 2 3 4))
(check-equal?
 (append:v2 (list 1) (list 2 3 4))
 (list 1 2 3 4))
(check-equal?
 (append:v2 (list 1 2) (list 3 4))
 (list 1 2 3 4))
(check-equal?
 (append:v2 (list 1 2 3) (list 4))
 (list 1 2 3 4))
(check-equal?
 (append:v2 (list 1 2 3 4) (list))
 (list 1 2 3 4))

(check-equal?
 (filter:v1 (λ (x) (not (= x 0))) '(0 1 0 2 3 0))
 '(1 2 3))
(check-equal?
 (filter:v1 (λ (x) (= x 1)) '(0 1 0 1 3 1))
 '(1 1 1))

(check-equal?
 (filter:v2 (λ (x) (not (= x 0))) '(0 1 0 2 3 0))
 '(1 2 3))
(check-equal?
 (filter:v2 (λ (x) (= x 1)) '(0 1 0 1 3 1))
 '(1 1 1))


;;----------------------- flodl ---------------------------
; Demonstrate how to abstract out a commond pattern
; from the following functions
;; --------------------------------------------------------
(define (reverse:v1 l)
  (define (rev-iter acc l)
    (cond
      [(empty? l) acc]
      [else (rev-iter (cons (first l) acc) (rest l))]))
  (rev-iter empty l))

(define (concat-nums:v1 nums)
  (define (concat-iter acc l)
    (cond
      [(empty? l) acc]
      [else (concat-iter
             (string-append acc " " (number->string (first l)))
             (rest l))]))
  (concat-iter ">" nums))

(define (foldl step base-case l)
  (define (foldl-iter acc l)
    (cond
      [(empty? l) acc]
      [else (foldl-iter (step (first l) acc) (rest l))]))
  (foldl-iter base-case l))

(define (reverse:v2 l)
  (foldl cons empty l))

(define (concat-nums:v2 l)
  (define (step n acc)
    (string-append acc " " (number->string n)))
  (foldl step ">" l))

(check-equal?
 (reverse:v1 '())
 '())
(check-equal?
 (reverse:v1 '(1))
 '(1))
(check-equal?
 (reverse:v1 '(1 2 3))
 '(3 2 1))

(check-equal?
 (reverse:v2 '())
 '())
(check-equal?
 (reverse:v2 '(1))
 '(1))
(check-equal?
 (reverse:v2 '(1 2 3))
 '(3 2 1))