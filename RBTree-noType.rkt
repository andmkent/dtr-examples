#lang racket

(require (for-syntax syntax/parse))

(define-syntax (with stx)
  (syntax-parse stx
    [(_ [((~datum R:) h1:id
                      ((~datum R:) h2:id ilhs:id ival:id irhs:id)
                      val:id
                      rhs:id)
         node]
        body ...)
     #'(let ([h1 (second node)]
             [h2 (second (third node))]
             [ilhs (third (third node))]
             [ival (fourth (third node))]
             [irhs (fifth (third node))]
             [val (fourth node)]
             [rhs (fifth node)])
         body ...)]
    [(_ [((~datum R:) h1:id
                      lhs:id
                      val:id
                      ((~datum R:) h2:id ilhs:id ival:id irhs:id))
         node]
        body ...)
     #'(let ([h1 (second node)]
             [lhs (third node)]
             [h2 (second (fifth node))]
             [ilhs (third (fifth node))]
             [ival (fourth (fifth node))]
             [irhs (fifth (fifth node))]
             [val (fourth node)])
         body ...)]
    [(_ [((~datum Node:) c:id h:id lhs:id val:id rhs:id) node]
        body ...)
     #'(let ([c (first node)]
             [h (second node)]
             [lhs (third node)]
             [val (fourth node)]
             [rhs (fifth node)])
         body ...)]
    [(_ [((~datum R:) h:id lhs:id val:id rhs:id) node]
        body ...)
     #'(let ([h (second node)]
             [lhs (third node)]
             [val (fourth node)]
             [rhs (fifth node)])
         body ...)]
    [(_ [((~datum B:) h:id lhs:id val:id rhs:id) node]
        body ...)
     #'(let ([h (second node)]
             [lhs (third node)]
             [val (fourth node)]
             [rhs (fifth node)])
         body ...)]
    ))

;; From Dependent ML: An Approach to Practical Programming with Dependent Types
;; by Hongwei Xi

(define (natural? nat)
  (exact-nonnegative-integer? nat))

(define (restore-L l val r)
  (cond
    [(and (eq? 'red (rb-color l))
          (eq? 'red (rb-color (rb-lhs l))))
     (with
      [(R: h1 (R: h2 l2 v2 r2) v1 r1) l]
      (mk-R
       (mk-B l2 v2 r2)
       v1
       (mk-B r1 val r)))]
    [(and (eq? 'red (rb-color l))
          (eq? 'red (rb-color (rb-rhs l))))       
     (with
      [(R: h1 l1 v1 (R: h2 l2 v2 r2)) l]
      (mk-R
       (mk-B l1 v1 l2)
       v2
       (mk-B r2 val r)))]
    [else ;; RBTree*
     (mk-B l val r)]))

(define (restore-R l val r)
  (cond
    [(and (eq? 'red (rb-color r))
          (eq? 'red (rb-color (rb-lhs r))))
     (with
      [(R: h1 (R: h2 l2 v2 r2) v1 r1) r]
      (mk-R
       (mk-B l val l2)
       v2
       (mk-B r2 v1 r1)))]
    [(and (eq? 'red (rb-color r))
          (eq? 'red (rb-color (rb-rhs r))))       
     (with
      [(R: h1 l1 v1 (R: h2 l2 v2 r2)) r]
      (mk-R
       (mk-B l val l1)
       v1
       (mk-B l2 v2 r2)))]
    [else ;; RBTree*
     (mk-B l val r)]))

(define (mk-R tree1 int tree2)
  (list 'red (second tree1) tree1 int tree2))

(define (mk-B tree1 int tree2)
  (list 'black (+ 1 (cadr tree2)) tree1 int tree2))

(define rb-color first)
(define rb-height second)
(define rb-lhs third)
(define rb-value fourth)
(define rb-rhs fifth)

(define (insert int rbt)
  (define (ins t)
    (cond
      [(= (length t) 2)
       (mk-R t int t)]
      [else
       (with
        [(Node: c h lhs v rhs) t]
        (cond
          [(eq? 'black c)
           (cond
             [(< int v)
              (restore-L (ins lhs) v rhs)]
             [(> int v)
              (restore-R lhs v (ins rhs))]
             [else t])]
          [else ;; red c
           (cond
             [(< int v)
              (mk-R (ins lhs) v rhs)]
             [(> int v)
              (mk-R lhs v (ins rhs))]
             [else t])]))]))
  (blacken-root (ins rbt)))


(define (blacken-root res)
  (cond
    [(= (length res) 2)
     res]
    [(eq? 'black (car res))
     res]
    [else (list 'black
                (+ 1 (second (third res)))
                (third res)
                (fourth res)
                (fifth res))]))

(define (build-rbtree ls-int)
  (for/fold ([rbt (list 'black 0)])
            ([int (in-list ls-int)])
    (insert int rbt)))
