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
  (Refine [bn : (List Black Natural Zero RBTree String RBTree)]
         (= (second bn) (+ 1 (second (fourth bn))))
         (= (second bn) (+ 1 (second (sixth bn))))))

(define-type RNode
  (Refine [rn : (List Red Natural Natural RBTree String RBTree)]
         (= (second rn) (second (fourth rn)))
         (= (second rn) (second (sixth rn)))
         (= (third rn) (+ (first (fourth rn)) (first (sixth rn))))))

(define-type RBTree
  (U RNode BNode MTNode))

;; Need function to remove or add nodes to RBTrees.
