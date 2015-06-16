#lang typed/racket

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
             [h2 (second (second node))]
             [ilhs (third (second node))]
             [ival (fourth (second node))]
             [irhs (fifth (second node))]
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
             [h2 (second (fourth node))]
             [ilhs (third (fourth node))]
             [ival (fourth (fourth node))]
             [irhs (fifth (fourth node))]
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


(define-type Int Integer)
(define-type Nat Natural)
(: natural? (-> Any Boolean : Nat))
(define (natural? nat)
  (exact-nonnegative-integer? nat))

(define-type Color (U 'black 'red))




(define-type Leaf
  (List 'black Zero))

(define-type BNode
  (Refine [bn : (List 'black Nat RBTree* Int RBTree*)]
         (= (second bn) (+ 1 (second (third bn))))
         (= (second bn) (+ 1 (second (fifth bn))))))


(define-type BTree
  (U BNode Leaf))

(define-type RNode
  (Refine [rn : (List 'red Nat BTree Int BTree)]
         (= (second rn) (second (third rn)))
         (= (second rn) (second (fifth rn)))))

(define-type RBTree
  BTree)

(define-type RBTree*
  (U RNode BNode Leaf))

(define-type R-LRNode ;; double check ref
  (Refine [rn : (List 'red Nat RNode Int BTree)]
          (= (second rn) (second (third rn)))
          (= (second rn) (second (fifth rn)))))

(define-type R-RRNode ;; double check ref
  (Refine [rn : (List 'red Nat BTree Int RNode)]
          (= (second rn) (second (third rn)))
          (= (second rn) (second (fifth rn)))))

(define-type RRNode (U R-LRNode R-RRNode))

(: restore-L (~> ([l : (U RRNode RBTree*)]
                  [val : Int]
                  [r : (Refine [r : RBTree*]
                               (= (second r) (second l)))])
                 (Refine [t : RBTree*]
                         (= (second t) (+ 1 (second l))))))
(define (restore-L l val r)
  (cond
    [(eq? 'red (rb-color (rb-lhs l)))
     (with
      [(R: h1 (R: h2 l2 v2 r2) v1 r1) l]
      (error 'do-stuff))]
    [(eq? 'red (rb-color (rb-rhs l)))
     (with
      [(R: h1 l1 v1 (R: h2 l2 v2 r2)) l]
      (error 'do-stuff))]
    [else ;; RBTRee*
     (error 'just-blacken?-or-make-black-node?)]))

(: restore-R (~> ([r : (Refine [r : RBTree*]
                               (= (second r) (second l)))]
                  [val : Int]
                  [l : (U RRNode RBTree*)])
                 (Refine [t : RBTree*]
                         (= (second t) (+ 1 (second l))))))
(define (restore-R l val r)
  (error 'foo))

(: mk-R (~> ([t1 : BTree]
             [int : Int]
             [t2 : (Refine [t : BTree]
                           (= (second t1) (second t2)))])
                  RNode))
(define (mk-R height tree1 int tree2)
  (list 'red (rb-height tree1) tree1 int tree2))

;; mk-B

(define-predicate leaf? Leaf)

(define rb-color first)
(define rb-height second)
(define rb-lhs third)
(define rb-value fourth)
(define rb-rhs fifth)


(define (black? t)
  (eq? 'black (car t)))
(define (red? t)
  (eq? 'red (car t)))

(: insert (-> Int RBTree RBTree))
(define (insert int rbt)
  (: ins (~> ([t : RBTree*])
             (Refine [t* : RBTree*]
                     (= (second t) (second t*)))))
  (define (ins t)
    (cond
      [(leaf? t)
       (mk-R 0 t int t)]
      [else
       (with
        [(Node: c h lhs v rhs) t]
        (cond
          [(eq? 'black c)
           (cond
             [(< int v) 
              (restore-L (ins lhs) v rhs)]
             [(> int (rb-value t))
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

(: blacken-root (-> (U RBTree RRNode) RBTree))
(define (blacken-root res)
  (if (eq? 'black (car res))
      res
      (cons 'black (cdr res))))

(: build-rbtree (-> (Listof Int) RBTree))
(define (build-rbtree ls-int)
  (for/fold ([rbt (list 0 0)])
            ([int (in-list ls-int)])
    (insert int rbt)))
