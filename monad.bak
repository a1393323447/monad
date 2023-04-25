#lang racket

(require rackunit)

(struct heap (data) #:transparent)
(struct handle (id) #:transparent)
(struct effect (state result) #:transparent)

(define (empty-heap) (heap (hash)))

(define/contract (heap-alloc hp init)
  (-> heap? number? effect?)
  (define data (heap-data hp))
  (define next-id (hash-count data))
  (define new-handle (handle next-id))
  (define new-data (hash-set data new-handle init))
  (define new-heap (heap new-data))
  (effect new-heap new-handle))

(define/contract (heap-read hp hd)
  (-> heap? handle? number?)
  (hash-ref (heap-data hp) hd))

(define/contract (heap-write hp hd val)
  (-> heap? handle? number? effect?)
  (define data (heap-data hp))
  (define new-data (hash-set data hd val))
  (define new-heap (heap new-data))
  (effect new-heap (void)))

(define (run-state op) (effect-state (op (empty-heap))))
(define expected-heap
  (heap (hash (handle 0) 1
            (handle 1) 2
            (handle 2) 3)))

;;------------------------------ Program 0 --------------------------------
(define/contract (prog0 hp0)
  (-> heap? effect?)
  ; allocate x
  (define eff-x (heap-alloc hp0 1))
  (define x (effect-result eff-x))
  (define hp1 (effect-state eff-x))
  ; allocate y
  (define eff-y (heap-alloc hp1 2))
  (define y (effect-result eff-y))
  (define hp2 (effect-state eff-y))
  ; read x y
  (define x-val (heap-read hp2 x))
  (define y-val (heap-read hp2 y))
  ; allocate z
  (heap-alloc hp2 (+ x-val y-val)))

(check-equal? (run-state prog0) expected-heap)
;;-------------------------------------------------------------------------

;;------------------------------ Program 2 --------------------------------
(define/contract (def val)
  (-> number? (-> heap? effect?))
  (lambda (in-hp)
    (heap-alloc in-hp val)))

(define/contract (add x y)
  (-> handle? handle? (-> heap? effect?))
  (lambda (in-hp)
    (define x-val (heap-read in-hp x))
    (define y-val (heap-read in-hp y))
    (define z-val (+ x-val y-val))
    ((def z-val) in-hp)))

(define/contract (prog1 hp0)
  (-> heap? effect?)
  ; allocate x
  (define eff-x ((def 1) hp0))
  (define x (effect-result eff-x))
  (define hp1 (effect-state eff-x))
  ; allocate y
  (define eff-y ((def 2) hp1))
  (define y (effect-result eff-y))
  (define hp2 (effect-state eff-y))
  ; define y with val x + y 
  ((add x y) hp2))

(check-equal? (run-state prog1) expected-heap)
;;-------------------------------------------------------------------------

;;------------------------------ Program 3 --------------------------------
(define/contract (operation? op)
  (-> (-> heap? effect?) boolean?)
  #t)

(define/contract (cont-op? cont)
  (-> (-> any/c operation?) boolean?)
  #t)

(define/contract (bind-wrong op1 op2)
  (-> operation? operation? operation?)
  ; new operation
  (lambda (in-hp)
    (define op1-eff (op1 in-hp))
    (define op1-hp (effect-state op1-eff))
    (define op1-res (effect-result op1-eff))
    (op2 op1-hp)))

(define/contract (bind op cont)
  (-> operation? cont-op? operation?)
  ; new operation
  (lambda (in-hp)
    (define op-eff (op in-hp))
    (define op-hp (effect-state op-eff))
    (define op-res (effect-result op-eff))
    ((cont op-res) op-hp)))

(define prog3
  (bind (def 1)
    (λ (x)
      (bind (def 2)
        (λ (y)
          (add x y))))))

(check-equal? (run-state prog3) expected-heap)
;;-------------------------------------------------------------------------

;;------------------------------ Program 4 --------------------------------
(define-syntax do
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (λ (var) (do rest ...)))]
    ; No binding operator, just ignore the return value
    {(_ mexp rest ...)        (bind mexp (λ (_) (do rest ...)))}))

(define prog4
  (do
    x <- (def 1)
    y <- (def 2)
    (add x y)))
(check-equal? (run-state prog4) expected-heap)
;;-------------------------------------------------------------------------

;;-------------------- Summary: the Monad -------------------------------------------
;; A monad is a functional pattern which can be categorized of two base combinators:
;; - Bind: combines two effectful operations O1 and O2. Operation O1 produces a value
;;   that consumed by operation O2
;; - Pure: converts a pure value to a monadic operation, which can then be chained
;;   with bind.
;;-----------------------------------------------------------------------------------

(define (pure v)
  (lambda (hp) (effect hp v)))

(define prog5
  (do
    x-val <- (pure (- 2 1))
    x <- (def x-val)
    y <- (def 2)
    (add x y)))

(check-equal? (run-state prog5) expected-heap)

;;------------------------------ Error Handling ------------------------------------
(struct return-ok (val) #:transparent)
(struct return-err (val) #:transparent)

(define (result? res) 
  (or (return-ok? res) 
      (return-err? res)))

(define (h-bind res cont)
  (cond
    [(return-err? res) res]
    [else (cont (return-ok-val res))]))

(define (h-pure v)
  (return-ok v))

(define (safe-/ x y)
  (cond [(= 0 y) (return-err 'division-by-zero)]
          [else (return-ok (/ x y))]))

(define-syntax try-impl
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (h-bind mexp (lambda (var) (try-impl rest ...)))]
    ; No binding operator, just ignore the return value
    [(_ mexp rest ...)        (h-bind mexp (lambda (_) (try-impl rest ...)))]))

(define (final-handler res err-handler)
  (cond
    [(return-ok? res) (return-ok-val res)]
    [(return-err? res) (err-handler (return-err-val res))]))   

(define-syntax try
  (syntax-rules (catch)
    [(_ ops ... catch (<err>) (body ...))
     (final-handler
      (try-impl ops ...)
      (λ (<err>) body ...))]))

(define a
  (try
   x <- (safe-/ 2 1)
   y <- (safe-/ 4 2)
   (h-pure (+ x y))

   catch (err) {
     (printf "err: ~a\n" err)
   }))
(check-equal? a 4)

(try
   x <- (safe-/ 2 1)
   y <- (safe-/ 4 2)
   z <- (h-pure (- x y))
   (safe-/ (+ x y) z)

   catch (err) {
     (printf "err: ~a\n" err)
   })
