#lang racket

(define (map f lst)
  (define (map-iter l acc)
    (cond
      [(empty? l) (reverse acc)]
      [else (map-iter (rest l) (cons (f (first l)) acc))]))
  (map-iter lst empty))

(define (map:v2 f lst)
  (define (map-iter l acc)
    (cond
      [(empty? l) (acc empty)]
      [else (map-iter
             (rest l)
             (λ (new-list)
               (acc (cons (f (first l)) new-list))))]))
  (map-iter lst identity))

; unoptimized
; (define (rec v)
;   (cond
;     [(base-case? v) (base v)]
;     [else (step v (rec (dec v)))]))

; optimized
; (define (rec v)
;   (define (rec-aux accum v)
;     (cond
;       [(base-case? v) (accum (base v))]
;       [else
;        (rec-aux
;         (λ (x) (accum (step v x)))
;         (dec v))]))
;   (rec-aux identity v))

(require rackunit)

(check-equal? (list 2 4 6)
             (map (λ (x) (* x 2)) (list 1 2 3)))

(check-equal? (list 2 4 6)
             (map:v2 (λ (x) (* x 2)) (list 1 2 3)))
