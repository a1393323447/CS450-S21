#lang racket

;;----------------------------------------------------
; This file implement a BST and some operation on it
; including insert, delete and search
;
; This file also implement tree-foldr and tree-foldl
; and a helper function list->bst
;;----------------------------------------------------

(require data/order)

; BST structure
(struct node (l r v))

; insert a value to bst and return the new bst
; non tail recursive
(define (bst-insert:v1 tree v)
  (cond
    [(equal? tree null) (node null null v)]
    [else
     (define cur-v (node-v tree))
     (define cur-l (node-l tree))
     (define cur-r (node-r tree))
     (cond
       [(< v cur-v) (node (bst-insert:v1 cur-l v) cur-r cur-v)]
       [else (node (node cur-l (bst-insert:v1 cur-r v) cur-v))])]))

; tail recursive
(define (bst-insert:v2 tree v)
  (define (bst-iter root acc)
    (cond
      [(equal? root null) (acc (node null null v))]
      [else
       (define cur-v (node-v root))
       (define cur-l (node-l root))
       (define cur-r (node-r root))
       (cond
         [(< v cur-v) (bst-iter cur-l
                                  (λ (sub-t) (acc (node sub-t cur-r cur-v))))]
         [else (bst-iter cur-r
                           (λ (sub-t) (acc (node cur-l sub-t cur-v))))])]))
  (bst-iter tree identity))

; tree-foldr: a function similar to foldr
; tail recursive 
(define (tree-foldr lstep estep rstep cmp base-case tree)
  (define (foldr-iter root acc)
    (cond
      [(empty? root) (acc base-case)]
      [else
       (define cur-v (node-v root))
       (define cur-l (node-l root))
       (define cur-r (node-r root))
       (define ord (cmp cur-v))
       (match ord
         ['< (foldr-iter cur-l
                                  (λ (sub-t) (acc (lstep sub-t cur-r cur-v))))]
         ['= (acc (estep cur-l cur-r cur-v))]
         ['> (foldr-iter cur-r
                                  (λ (sub-t) (acc (rstep cur-l sub-t cur-v))))])]))
  (foldr-iter tree identity))

; tree-foldr: a function similar to foldl
; tail recursive
(define (tree-foldl lstep estep rstep cmp base-case tree)
  (define (foldr-iter root acc)
    (cond
      [(empty? root) acc]
      [else
       (define cur-v (node-v root))
       (define cur-l (node-l root))
       (define cur-r (node-r root))
       (define ord (cmp cur-v))
       (match ord
         ['< (foldr-iter cur-l (lstep acc cur-r))]
         ['= (acc (estep cur-l cur-r cur-v))]
         ['> (foldr-iter cur-r (rstep cur-l acc))])]))
  (foldr-iter tree base-case))

; implement bst-insert with tree-foldr
(define (bst-insert:v3 tree v)
  (define cmp (curry real-order v))
  (tree-foldr node node node cmp (node null null v) tree))

(define bst-insert bst-insert:v3)

; search if a v exsists in the given bst
(define (bst-search tree v)
  (cond
    [(equal? tree null) #f]
    [else
     (define cur-v (node-v tree))
     (define cur-l (node-l tree))
     (define cur-r (node-r tree))
     (cond
       [(< v cur-v) (bst-search cur-l v)]
       [(= v cur-v) #t]
       [(> v cur-v) (bst-search cur-r v)])]))

; delete a node contains value v and return the new tree
; implement with tree-foldr
(define (bst-delete tree v)
  ; a helper function for insert a tree to another tree
  (define (bst-insert-node t1 t2)
    (cond
    [(empty? t2) t1]
    [else
     (define cmp (curry real-order (node-v t2)))
     (tree-foldr node node node cmp t2 t1)]))
  ; when we find the node needed to delete
  ; we insert its r-tree to l-tree to construct a new tree
  (define (estep l r cur-v)
    (bst-insert-node l r))
  (define cmp (curry real-order v))
  (tree-foldr node estep node cmp (node null null null) tree))

; a helper function converting a list to a bst
(define (list->bst lst)
  (foldr (λ (v tree) (bst-insert tree v)) null lst))

