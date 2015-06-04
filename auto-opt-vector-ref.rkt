#lang typed/racket

(define x : Natural (random 1000))
(define v : (Vectorof Natural) (build-vector (random 100) (λ _ (random 10))))

(if (< -1 x (vector-length v))
    (vector-ref v x) ;; this should automatically optimize to unsafe-vector-ref
    "invalid bound!")

;; this should auto optimize too!
#;(if (< x (vector-length v))
    (vector-ref v x)
    "invalid bound!")
