#lang racket

(require rackunit)

; stream = (cons some-value (thunk stream))
(define (stream init step)
  (define (stream-iter value)
    (cons value (thunk (stream-iter (step value)))))
  (stream-iter init))

; stream utility
(define (stream-get s)
  (car s))

(define (stream-next s)
  ((cdr s)))

(define (stream-skip s n)
  (cond
    [(equal? n 0) s]
    [else (stream-skip (stream-next s) (sub1 n))]))

(define (stream-ref s n)
  (stream-get (stream-skip s n)))

(define (count-until s pred)
  (define (count s cnt)
    (cond
      [(pred (stream-get s)) (count (stream-next s) (add1 cnt))]
      [else cnt]))
  (count s 0))

(define (stream-map s f)
  (define mf (f (stream-get s)))
  (define ns (stream-next s))
  (define th (thunk (stream-map ns f)))
  (cons mf th))

(define (stream-filter s to-keep?)
  (define f (stream-get s))
  (define ns (stream-next s))
  (cond
    [(to-keep? f) (cons f (thunk (stream-filter ns to-keep?)))]
    [else (stream-filter ns to-keep?)]))

(define (stream-zip s u)
  (define sf (stream-get s))
  (define uf (stream-get u))
  (define ns (stream-next s))
  (define nu (stream-next u))
  (cons
   (cons sf uf)
   (thunk (stream-zip ns nu))))

(define (stream-interleave s u)
  (define sf (stream-get s))
  (define ns (stream-next s))
  (define th (thunk (stream-interleave u ns)))
  (cons sf th))

(define (stream-merge f s u)
  (define sf (stream-get s))
  (define uf (stream-get u))
  (define mv (f sf uf))
  (define ns (stream-next s))
  (define nu (stream-next u))
  (define th (thunk (stream-merge f ns nu)))
  (cons mv th))

(define (stream-take s n)
  (define (stream-iter cur-s acc n)
    (cond
      [(= n 0) (acc empty)]
      [else
       (define cur-v (stream-get cur-s))
       (define ns (stream-next cur-s))
       (stream-iter ns
                    (λ (new-lst) (acc (cons cur-v new-lst)))
                    (sub1 n))]))
  (stream-iter s identity n))

(define (stream-fold step basic-case s)
  (define th (thunk (stream-map step s)))
  (cons basic-case th))

(define (stream-enum s)
  (define idxs (stream 0 add1))
  (stream-zip idxs s))

; unit tests
(define powers-of-two
  (stream 1 (λ (x) (* 2 x))))
(check-equal?
 (stream-ref powers-of-two 0) 1)
(check-equal?
 (stream-ref powers-of-two 1) 2)
(check-equal?
 (stream-ref powers-of-two 2) 4)


(define natural
  (stream 0 add1))
(check-equal?
 (count-until powers-of-two (curryr < 8)) 3)
(check-equal?
 (count-until powers-of-two (curryr < 0)) 0)


(define even
  (stream-map natural (λ (x) (* 2 x))))
(check-equal? (stream-ref even 0) 0)
(check-equal? (stream-ref even 1) 2)
(check-equal? (stream-ref even 2) 4)
(check-equal? (stream-ref even 3) 6)


(define greater-than-8
  (stream-filter natural (curryr > 8)))
(check-equal? (stream-ref greater-than-8 0) 9)
(check-equal? (stream-ref greater-than-8 1) 10)
(check-equal? (stream-ref greater-than-8 2) 11)
(check-equal? (stream-ref greater-than-8 3) 12)


(define nat-even
  (stream-zip natural even))
(check-equal? (stream-ref nat-even 0) '(0 . 0))
(check-equal? (stream-ref nat-even 1) '(1 . 2))
(check-equal? (stream-ref nat-even 2) '(2 . 4))
(check-equal? (stream-ref nat-even 3) '(3 . 6))

(define nat-even-inter
  (stream-interleave natural even))
(check-equal? (stream-ref nat-even-inter 0) 0)
(check-equal? (stream-ref nat-even-inter 1) 0)
(check-equal? (stream-ref nat-even-inter 2) 1)
(check-equal? (stream-ref nat-even-inter 3) 2)

(define merge-nat-even
  (stream-merge
   (λ (n e) (+ n e))
   natural even))
(check-equal? (stream-ref merge-nat-even 0) 0) ; 0 + 0
(check-equal? (stream-ref merge-nat-even 1) 3) ; 1 + 2
(check-equal? (stream-ref merge-nat-even 2) 6) ; 2 + 4
(check-equal? (stream-ref merge-nat-even 3) 9) ; 3 + 6

(check-equal? (stream-take even 0) '())
(check-equal? (stream-take even 1) '(0))
(check-equal? (stream-take even 2) '(0 2))
(check-equal? (stream-take even 3) '(0 2 4))

