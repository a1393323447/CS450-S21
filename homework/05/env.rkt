#lang racket

(provide (all-defined-out))

(struct heap (data) #:transparent)
(struct handle (id) #:transparent)
; effect: state: heap   res: evaluated val
(struct eff (state res) #:transparent)

(define (eff-destruct e)
  (values (eff-state e)
          (eff-res e)))

(define (empty-heap) (heap (hash)))

(define (heap-alloc h val)
  (define data (heap-data h))
  (define id (hash-count data))
  (define hd (handle id))
  (define new-heap (heap (hash-set data hd val)))
  (eff new-heap hd))

(define (heap-get hp hd)
  (hash-ref (heap-data hp) hd))

(define (heap-put hp hd val)
  (define data (heap-data hp))
  (cond
    [(hash-has-key? data hd) (heap (hash-set data hd val))]
    [else (error "heap-put: Unkown handle")]))

; parent: handle
; locals: hash-table
(struct frame (parent locals) #:transparent)

(define (root-frame locals) (frame #f locals))

(define (frame-put frm var val)
  (define parent (frame-parent frm))
  (define locals (frame-locals frm))
  (define new-locals (hash-set locals var val))
  (frame parent new-locals))

(define (frame-get frm var)
  (define locals (frame-locals frm))
  (hash-ref locals var #f))

(define (environ-init root-frm)
  (heap-alloc (empty-heap) root-frm))

; env is a handle to a frame
; return a new heap
(define (environ-put hp env var val)
  (define old-frm (heap-get hp env))
  (define new-frm (frame-put old-frm var val))
  (heap-put hp env new-frm))

; env is a handle to a frame
; return an eff struct
; which contains a new heap and a new env
(define (environ-push hp env var val)
  (define locals (hash var val))
  (define new-frm (frame env locals))
  (heap-alloc hp new-frm))

(define (environ-push-pairs hp env pairs)
  (define locals (insert-pairs (hash) pairs))
  (define new-frame (frame env locals))
  (heap-alloc hp new-frame))

(define (environ-get hp env var)
  (define frm (heap-get hp env))
  (define parent (frame-parent frm))
  (define result (frame-get frm var))
  (cond
    [result result]
    [parent (environ-get hp parent var)]
    [else (error (format "Variable ~a is not defined" var))]))

(define (insert-pair pair table)
    (define pa (car pair))
    (define pb (cdr pair))
    (hash-set table pa pb))

(define (insert-pairs table pairs)
    (foldr insert-pair table pairs))

