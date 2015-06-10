#lang typed/racket
(require typed/safe/ops)

(define x : Natural (random 1000))
(define v : (Vectorof Natural) (build-vector (random 100) (λ _ (random 10))))

(if (< -1 x (vector-length v))
    (safe-vector-ref v x)
    "invalid bound!")

;; this should work too!
(if (< x (vector-length v))
    (safe-vector-ref v x)
    "invalid bound!")
