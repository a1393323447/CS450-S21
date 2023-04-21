#lang racket

(define (member elem lst)
  (cond
    [(empty? lst) #f]
    [(equal? (first lst) elem) #t]
    [else (member elem (rest lst))]))

(define (string-prefix? s p)
  (define (str-first str)
    (string-ref str 0))
  (define (str-rest str)
    (substring str 1))
  (define (str-empty? str)
    (equal? str ""))
  (cond [(str-empty? p) #t]
        [(str-empty? s) #f]
        [(equal? (str-first p) (str-first s))
         (string-prefix? (str-rest s) (str-rest p))]
        [else #f]))

(define (match-prefix? p s-lst)
  (cond [(empty? s-lst) #f]
        [(string-prefix? (first s-lst) p) #t]
        [else (match-prefix? p (rest s-lst))]))


(define (exists? predicate lst)
  (cond [(empty? lst) #f]
        [(predicate (first lst)) #t]
        [else (exists? predicate (rest lst))]))

(define (curry-exists? eq?)
  (define (c-exists? elem lst)
    (cond [(empty? lst) #f]
          [(eq? (first lst) elem) #t]
          [else (c-exists? elem (rest lst))]))
  c-exists?)

(define member:v2 (curry-exists? =))
(define match-prefix?:v2 (curry-exists? string-prefix?))

(require rackunit)

(check-true (member 1 (list 0 2 1)))
(check-false (member 2 (list 4 5 6)))
(check-false (member empty empty))
(check-true (exists? (λ (x) (= x 1)) (list 0 2 1)))
(check-true (member:v2 1 (list 0 2 1)))
(check-false (member:v2 2 (list 4 5 6)))
(check-false (member:v2 empty empty))

(check-true (string-prefix? "Racket" "R"))
(check-true (string-prefix? "Racket" "Rac"))
(check-true (string-prefix? "" ""))
(check-false (string-prefix? "" "R"))

(check-true (match-prefix? "R" (list "foo" "Racket")))
(check-false (match-prefix? "R" (list "foo" "bar")))
(check-true (exists? (λ (s) (string-prefix? s "R")) (list "foo" "Racket")))
(check-true (match-prefix?:v2 "R" (list "foo" "Racket")))
(check-false (match-prefix?:v2 "R" (list "foo" "bar")))
