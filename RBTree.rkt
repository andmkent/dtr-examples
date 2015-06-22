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


(define-type Int Integer)
(define-type Nat Natural)

(: natural? (-> Any Boolean : Nat))
(define (natural? nat)
  (exact-nonnegative-integer? nat))

(define-type Color (U Black Red))

(define-type Black
  'black)

(define-type Red
  'red)

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

(define-type RBTree*
  (U RNode BTree))

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
                         ;; BUG If below were (= (second t) (+ 1 (second l)))
                         ;; ins does not type-check
                         (= (second t) (+ 1 (second r))))))
(define (restore-L l val r)
  (error 'WORKING) #;
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

(: restore-R (~> ([l : (Refine [l : RBTree*]
                               (= (second l) (second r)))]
                  [val : Int]
                  [r : (U RRNode RBTree*)])
                 (Refine [t : RBTree*]
                         (= (second t) (+ 1 (second l)))
                         (= (second t) (+ 1 (second r))))))
(define (restore-R l val r)
  (error 'WORKING) #;
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

(: mk-R (~> ([t1 : RBTree*]
             [int : Int]
             [t2 : (Refine [t2 : RBTree*]
                           (= (second t1) (second t2)))])
            (Refine [r : (U RRNode RNode)]
                    (= (second t1) (second r))
                    (= (second t2) (second r))
                    (or
                     (and (t1 -: BTree)
                          (t2 -: BTree)
                          (r -: RNode))
                     (r -: RRNode)))))
(define (mk-R tree1 int tree2)
  (error 'stuff) #;
  (list 'red (second tree1) tree1 int tree2))

#;#;
(: mk-B (~> ([t1 : RBTree*]
             [int : Int]
             [t2 : (Refine [t : RBTree*]
                           (= (second t1) (second t2)))])
            (Refine [r : BNode]
                    (= (+ 1 (second t2)) (second r)))))
(define (mk-B tree1 int tree2)
  (list 'black (+ 1 (cadr tree2)) tree1 int tree2))

(define-predicate leaf? Leaf)

(define rb-color first)
(define rb-height second)
(define rb-lhs third)
(define rb-value fourth)
(define rb-rhs fifth)

#;#;#;#;
(: black? (-> (Listof Any) Boolean))
(define (black? t)
  (eq? 'black (car t)))
(: red? (-> (Listof Any) Boolean))
(define (red? t)
  (eq? 'red (car t)))

(: insert (-> Int BTree BTree))
(define (insert int rbt)
  (: ins (~> ([t : RBTree*])
             (Refine [t* : (U RRNode RBTree*)]
                     (= (second t) (second t*))
                     (or
                      (and (t -: BTree)
                           (t* -: RBTree*))
                      (and ((car t) -: Red)
                           (t* -: (U RRNode RBTree*)))))))
  (define (ins t)
    (cond
      [(leaf? t)
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


(: blacken-root (-> (U RBTree* RRNode) BTree))
(define (blacken-root res)
  (cond
    [(leaf? res)
     res]
    [(eq? 'black (car res))
     res]
    [(= (second (third res))
        (second (fifth res)))
     (list 'black
           (+ 1 (second res))
           (third res)
           (fourth res)
           (fifth res))]
    [else (list 'black
                (+ 1 (second (third res)))
                (third res)
                (fourth res)
                (fifth res))]))

#;#; ;WORKING
(: build-rbtree (-> (Listof Int) RBTree))
(define (build-rbtree ls-int)
  (for/fold ([rbt : RBTree (list 'black 0)])
            ([int (in-list ls-int)])
    (insert int rbt)))
