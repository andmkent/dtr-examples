#lang typed/racket
(require typed/safe/ops)

(define-type (Vec a) (Vectorof a))
(define-type Int Integer)

(define-syntax-rule (safe-matrix-set! A r c val)
  (safe-vector-set! (safe-vector-ref A r) c val))

;; A[m×n] * B[n×p] = C[m×p]) 
(: matrix*
   (~> ([A : (Refine [A : (Vec (Refine [a : (Vec Int)] (= (len a) n)))] (= (len A) m))]
        [B : (Refine [B : (Vec (Refine [b : (Vec Int)] (= (len b) p)))] (= (len B) n))]
        [C : (Refine [C : (Vec (Refine [c : (Vec Int)] (= (len c) p)))] (= (len C) m))]
        [m : Natural]
        [n : Natural]
        [p : Natural])
       Void))
(define (matrix* A B C m n p)
  (let i-loop ([i : Natural 0])
    (when (< i m)
      (define A-i (safe-vector-ref A i))
      (define C-i (safe-vector-ref C i))
      (let j-loop ([j : Natural 0])
        (when (< j p)
          (let k-loop ([k : Natural 0] [sum : Integer 0])
            (cond
              [(< k n)
               (define A-i-k (safe-vector-ref A-i k))
               (define B-k-j (safe-vector-ref (safe-vector-ref B k) j))
               (k-loop (add1 i) (* A-i-k B-k-j))]
              [else
               (safe-matrix-set! C i j sum)]))
          (j-loop (add1 j))))
      (i-loop (add1 i)))))
