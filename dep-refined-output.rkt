#lang typed/racket

(: negate (~> ([x : (U Number Boolean)]) 
              (Refine [z : (U Number Boolean)]
                      (or (x z -: Number)
                          (x z -: Boolean)))))
(define negate
  (λ (a) (cond
           [(number? a) (* -1 a)]
           [else (not a)])))

(ann (negate #t) Boolean)

(ann (negate 42) Number)

(: foo (-> (U Number Boolean) Number))
(define foo 
  (λ (x) (let ([y (negate x)])
           (if (number? y)
               x
               0))))
