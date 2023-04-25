#lang racket

(require "env.rkt")
(require rackunit)

(define (bind heap-op cont)
  ; takes a heap -> produces an eff
  (lambda (hp)
    (define cur-eff (heap-op hp))
    (define res (eff-res cur-eff))
    (define new-hp (eff-state cur-eff))
    ((cont res) new-hp)))

(define (run-state h op) (eff-state (op h)))

#|
A stack-based evaluator

Operations:

def mult():
    x = pop()
    y = pop()
    push (x * y)

def prog():
    push(2)
    push(5)
    mult()
    push(2)
    mult()

State:
a list of numbers
|#

(define (pop)
  (λ (s)
    (eff (rest s) (first s))))

(define (push n)
  (λ (s)
    (eff (cons n s) (void))))

(define (mult)
  (bind (pop)
    (λ (x)
      (bind (pop)
        (λ (y)
          (push (* x y)))))))
(check-equal? (run-state (list 2 3) (mult)) '(6))

; this prog is pretty ugly
(define (prog1)
  (bind (push 2)
    (λ (_) (bind (push 5)
      (λ (_) (bind (mult)
        (λ (_) (bind (push 2)
          (λ (_) (mult))))))))))
(check-equal? (run-state empty (prog1)) '(20))

(define (seq op . ops)
  (cond [(empty? ops) op]
        [else (bind op (λ (_) (apply seq ops)))]))

(define (prog2)
  (seq (push 2)
       (push 5)
       (mult)
       (push 2)
       (mult)))
(check-equal? (run-state empty (prog2)) '(20))

; we could do better by using marco 
(define-syntax do
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (λ (var) (do rest ...)))]
    ; No binding operator, just ignore the return value
    {(_ mexp rest ...)        (bind mexp (λ (_) (do rest ...)))}))

; if we want to do some pure operations and assign it to a variable in do context
; we need to use this helper function which sets the res to v
; and keeps the s remain unchanged
(define (pure v)
  (λ (s) (eff s v)))

; now we can write the mul and prog in a more pretty way
(define (mul)
  (do
    x <- (pop)
    y <- (pop)
    z <- (pure (* x y))
    (push z)))

(define (prog3)
  (do
    (push 2)
    (push 5)
    (mul)
    (push 2)
    (mul)))
(check-equal? (run-state empty (prog3)) '(20))
