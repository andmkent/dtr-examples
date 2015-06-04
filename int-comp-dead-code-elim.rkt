#lang typed/racket

(: foo (-> (Refine [i : Integer] (< i 10)) Integer))
(define (foo x)
  (if (< x 11)
      42
      "dead code"))

(: bar (-> Integer Integer Integer))
(define (bar x y)
  (if (< x y)
      (if (< y x)
          "dead code"
          42)
      42))
