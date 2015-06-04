#lang typed/racket
(require typed/safe/ops)

(: foo (-> (Refine [i : Integer] (< i 10)) Integer))
(define (foo x)
  (if (< x 11)
      42
      "dead code"))