#lang typed/racket
(require typed/safe/ops)

(define v : (Vectorof Natural) (build-vector (random 100) (λ _ (random 10))))


(for/sum ([i : (Refine [i : Index] (≤ 0 i) (< i (len v)))
             (in-range (vector-length v))])
  (vector-ref v i)) ;; this automatically optimizes to unsafe-vector-ref