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
  (List Black Zero Zero))

(define-type BNode
  (Refine [bn : (List Black Natural Zero RBTree Integer RBTree)]
         (= (second bn) (+ 1 (second (fourth bn))))
         (= (second bn) (+ 1 (second (sixth bn))))))

(define-type RNode
  (Refine [rn : (List Red Natural Natural (U MTNode BNode) Integer (U MTNode BNode))]
         (= (second rn) (second (fourth rn)))
         (= (second rn) (second (sixth rn)))
         #;(= (third rn) (+ (first (fourth rn)) (first (sixth rn))))))


(: make-RNode (~> ([height : (Refine [height : Natural]
                                     (= height (second t1))
                                     (= height (second t1)))]
                   [viol : (Refine [viol : Natural] (= viol (+ (first t1) (first t2))))]
                   [t1 : (U MTNode BNode)]
                   [int : Integer]
                   [t2 : (U MTNode BNode)])
                  RNode))
(define (make-RNode height viol tree1 int tree2) (error 'foo) #;
  (list 1 height viol tree1 int tree2))

(define-type RBTree
  (U RNode BNode MTNode))

(define-predicate MTNode? MTNode)

(: restore (-> (List Natural Natural Natural RBTree Integer RBTree) RBTree))
(define (restore rbt)
  (match rbt
    [(list 1 h v (list 1 h1 v1 (list 1 h2 v2 a x b) y c) z d)
     (let ([k1 (list 0 (+ 1 (second a)) 0 a x b)]
            [k2 (list 0 (+ 1 (second c)) 0 c z d)])
       (list 1 (+ 1 (second a)) 0 k1 y k2))]
    [(list 1 h v (list 1 h1 v1 a x (list 1 h2 v2 b y c)) z d)
     (let ([k1 (list 0 (+ 1 (second a)) 0 a x b)]
            [k2 (list 0 (+ 1 (second c)) 0 c z d)])
       (list 1 (+ 1 (second a)) 0 k1 y k2))]
    [(list 1 h v a x (list 1 h1 v1 (list 1 h2 v2 b y c) z d))
     (let ([k1 (list 0 (+ 1 (second a)) 0 a x b)]
            [k2 (list 0 (+ 1 (second c)) 0 c z d)])
       (list 1 (+ 1 (second a)) 0 k1 y k2))]
    [(list 1 h v a x (list 1 h1 v1 b y (list 1 h2 v2 c z d)))
     (let ([k1 (list 0 (+ 1 (second a)) 0 a x b)]
            [k2 (list 0 (+ 1 (second c)) 0 c z d)])
       (list 1 (+ 1 (second a)) 0 k1 y k2))]
    #;[(list c h v a x b)
     (list 0 h 0 a x b)]))

(: RBinsert (-> Integer RBTree RBTree))
(define (RBinsert int tree)
  (cond
    [(MTNode? tree)
     (make-RNode (second tree) (first tree) tree int tree)]
    [(zero? (car tree))
     (cond
       [(< int (fifth tree))
        (restore (list 0 (second tree) 0 (RBinsert int (fourth tree)) (fifth tree) (sixth tree)))]
       [(> int (fifth tree))
        (restore (list 0 (second tree) 0 (fourth tree) (fifth tree) (RBinsert int (sixth tree))))]
       [else tree])]
    [(= 1 (car tree))
     (cond
       [(< int (fifth tree))
        (make-RNode (second tree) 0 (RBinsert int (fourth tree)) (fifth tree) (sixth tree))]
       [(> int (fifth tree))
        (make-RNode (second tree) 0 (fourth tree) (fifth tree) (RBinsert int (sixth tree)))]
       [else tree])]
    [else tree]))
