#lang typed/racket

;; From Dependent ML: An Approach to Practical Programming with Dependent Types
;; by Hongwei Xi



(: natural? (-> Any Boolean : Natural))
(define (natural? nat)
  (exact-nonnegative-integer? nat))

(define-type Color (U Black Red))

(define-type Black
  Zero)

(define-type Red
  One)

(define-type MTNode
  (List Black Zero))

(define-type BNode
  (Refine [bn : (List Black Natural RBTree Integer RBTree)]
         (= (second bn) (+ 1 (second (third bn))))
         (= (second bn) (+ 1 (second (fifth bn))))))

(define-type RNode
  (Refine [rn : (List Red Natural (U MTNode BNode) Integer (U MTNode BNode))]
         (= (second rn) (second (third rn)))
         (= (second rn) (second (fifth rn)))))


(: make-RNode (~> ([height : (Refine [height : Natural]
                                     (= height (second t1))
                                     (= height (second t1)))]
                   [t1 : (U MTNode BNode)]
                   [int : Integer]
                   [t2 : (U MTNode BNode)])
                  RNode))
(define (make-RNode height tree1 int tree2) (error 'foo) #;
  (list 1 height tree1 int tree2))

(define-type RBTree
  (U RNode BNode MTNode))

(define-predicate MTNode? MTNode)

(define rb-color first)

(define rb-height second)

(define rb-left third)

(define rb-value fourth)

(define rb-right fifth)

(: restore (-> (List Natural Natural RBTree Integer RBTree) RBTree))
(define (restore rbt)
  (match rbt
    [(list 1 h (list 1 h1 (list 1 h2 v2 a x b) y c) z d)
     (let ([k1 (list 0 (+ 1 (rb-height a)) a x b)]
            [k2 (list 0 (+ 1 (rb-height c)) c z d)])
       (list 1 (+ 1 (rb-height a)) k1 y k2))]
    [(list 1 h (list 1 h1 a x (list 1 h2 b y c)) z d)
     (let ([k1 (list 0 (+ 1 (rb-height a)) a x b)]
            [k2 (list 0 (+ 1 (rb-height c)) c z d)])
       (list 1 (+ 1 (rb-height a)) k1 y k2))]
    [(list 1 h a x (list 1 h1 (list 1 h2 b y c) z d))
     (let ([k1 (list 0 (+ 1 (rb-height a)) a x b)]
            [k2 (list 0 (+ 1 (rb-height c)) c z d)])
       (list 1 (+ 1 (rb-height a)) k1 y k2))]
    [(list 1 h a x (list 1 h1 b y (list 1 h2 c z d)))
     (let ([k1 (list 0 (+ 1 (rb-height a)) a x b)]
            [k2 (list 0 (+ 1 (rb-height c)) c z d)])
       (list 1 (+ 1 (rb-height a)) k1 y k2))]
    #;[(list c h v a x b)
     (list 0 h 0 a x b)]))

(: RBinsert (-> Integer RBTree RBTree))
(define (RBinsert int tree)
  (cond
    [(MTNode? tree)
     (make-RNode (rb-height tree) tree int tree)]
    [(zero? (car tree))
     (cond
       [(< int (rb-value tree))
        (restore (list 0 (rb-height tree) (RBinsert int (rb-left tree)) (rb-value tree) (rb-right tree)))]
       [(> int (rb-value tree))
        (restore (list 0 (rb-height tree) (rb-left tree) (rb-value tree) (RBinsert int (rb-right tree))))]
       [else tree])]
    [(= 1 (car tree))
     (cond
       [(< int (rb-value tree))
        (make-RNode (rb-height tree) (RBinsert int (rb-left tree)) (rb-value tree) (rb-right tree))]
       [(> int (rb-value tree))
        (make-RNode (rb-height tree) (rb-left tree) (rb-value tree) (RBinsert int (rb-right tree)))]
       [else tree])]
    [else tree]))

(: build-rbtree (-> (Listof Integer) RBTree))
(define (build-rbtree ls-int)
  (for/fold ([rbt (list 0 0)])
            ([int (in-list ls-int)])
    (RBinsert int rbt)))
