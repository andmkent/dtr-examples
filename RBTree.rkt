#lang typed/racket

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
                  [r : (Refine [r : (RBTree*)]
                               (= (second r) (second l)))])
                 (Refine [t : RBTree*]
                         (= (second t) (+ 1 (second l))))))

(: restore-R (~> ([r : (Refine [r : (RBTree*)]
                               (= (second r) (second l)))]
                  [val : Int]
                  [l : (U RRNode RBTree*)])
                 (Refine [t : RBTree*]
                         (= (second t) (+ 1 (second l))))))

(: mk-R (~> ([t1 : BTree]
             [int : Int]
             [t2 : (Refine [t : BTree]
                           (= (second t1) (second t2)))])
                  RNode))
(define (mk-R height tree1 int tree2)
  (list 'red (rb-height tree1) tree1 int tree2))



(define-predicate leaf? Leaf)

(define rb-color first)
(define rb-height second)
(define rb-left third)
(define rb-value fourth)
(define rb-right fifth)


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
      [(black? t)
       (cond
         [(< int (rb-value t)) 
          (restore-L (ins (rb-left t)) (rb-value t) (rb-right t))]
         [(> int (rb-value t))
          (restore-R (rb-left t) (rb-value t) (ins (rb-right t)))]
         [else t])]
      [else ;; red
       (cond
         [(< int (rb-value t))
          (mk-R (ins (rb-left t)) (rb-value t) (rb-right t))]
         [(> int (rb-value t))
          (mk-R (rb-left t) (rb-value t) (ins (rb-right t)))]
         [else t])]))

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
    (RBinsert int rbt)))
