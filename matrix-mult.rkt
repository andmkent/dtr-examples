#lang typed/racket
(require typed/safe/ops)

; Example taken from From Flop to Megaflops: Java for Technical Computing
; by Moreira et al.

(: matmul (~> ([A : (Refine [A : (Vectorof (Refine [a : (Vectorof Integer)] (= (len a) n)))] (= (len A) m))]
               [B : (Refine [B : (Vectorof (Refine [b : (Vectorof Integer)] (= (len b) p)))] (= (len B) n))]
               [C : (Refine [C : (Vectorof (Refine [c : (Vectorof Integer)] (= (len c) p)))] (= (len C) m))]
               [m : Natural]
               [n : Natural]
               [p : Natural])
                   Void))
#;(define (matmul vecA vecB vecC m n p)
  (let loop ([i : Natural 0] [j : Natural 0] [k : Natural 0])
    (if (< i m)
        (if (< j p)
            (if (< k n)
                (begin
                  (safe-vector-set! (safe-vector-ref vecC i) j
                                    (* (safe-vector-ref (safe-vector-ref vecA i) k)
                                       (safe-vector-ref (safe-vector-ref vecB k) j)))
                  (loop i j (+ 1 k)))
                (loop i (+ 1 j) 0))
            (loop (+ 1 i) 0 0))
        (void))))

(define (matmul vecA vecB vecC m n p)
  (let loop ([i : Natural 0] [j : Natural 0] [k : Natural 0])
    (cond
      [(< i m)
       (cond
         [(< j p)
          (cond
            [(< k n)
             (define vA (safe-vector-ref vecA i))
             (define vB (safe-vector-ref vecB k))
             (define vC (safe-vector-ref vecC i))
             (safe-vector-set! vC j
                               (* (safe-vector-ref vA k)
                                  (safe-vector-ref vB j)))
             (loop i j (+ 1 k))]
            [else (loop i (+ 1 j) 0)])]
         [else (loop (+ 1 i) 0 0)])])))
