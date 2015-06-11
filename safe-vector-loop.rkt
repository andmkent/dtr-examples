#lang typed/racket
(require typed/safe/ops)

(define v : (Vectorof Natural) (build-vector (random 100) (λ _ (random 10))))


;; we should support this and/or other convenient syntax =(
#;(define-type-syntax (Index-for v)
    (Refine [i : Integer] (≤ 0 i) (< i (len v))))

(for/sum ([i : (Refine [i : Index] (< i (len v)))
             (range (vector-length v))])
  (safe-vector-ref v i))

(for/sum ([i : (Refine [i : Index] (< i (len v)))
             (in-range (vector-length v))])
  (safe-vector-ref v i))
