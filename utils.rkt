#lang racket
(require "macro.rkt"
         rackunit
         racket/generator)
(provide (all-defined-out))

;;; -----------------------------------------------------------
;; Functional Stuff
;;; -----------------------------------------------------------
(def (repeat times func)
  (when (> times 0)
    (func)
    (repeat (sub1 times) func)))

(def (nequal? x y)
  (not (equal? x y)))

(def (neq? x y)
  (not (eq? x y)))

(def (flip fn)
  (lam (x y) (fn y x)))

(def (sum-list ls)
  (foldr + 0 ls))

(def (best fn ls)
  (match ls
    [(list)          'no-elem]
    [(cons fst rst)  (let ([winner fst])
                       (for ([obj rst]) (when (fn obj winner)
                                          (set! winner obj)))
                       winner)]))

(def (most fn ls)
  (match ls
    [(list)          'no-elem]
    [(cons fst rst)  (let ([res (list fst)]
                           [max (fn fst)])
                       (for ([obj rst])
                         (let ([score (fn obj)])
                           (cond [(> score max)
                                  (set! res (list obj))
                                  (set! max score)]
                                 [(= score max)
                                  (cons! obj res)]))))]))

;;; ------------------------------------------------------------
;;; Hash Tables
;;; ------------------------------------------------------------
(def (hash-set-many ht ls val)
  (for ([item ls])
    (set! ht (hash-set ht item val)))
  ht)

(define-syntax-rule (def-mem (f args ...) bodies ...)
  ;; replace define with a memoized version
  (define f
    ;; store the cache as a hash of args => result
    (let ([results (make-hash)])
      (lambda (args ...)
        (hash-ref! results
                   (list args ...)
                   (thunk (begin bodies ...)))))))

;;; ------------------------------------------------------------
;; Testing Functions
;;; ------------------------------------------------------------
(def (fib n)
  (cond [(= 0 n) 1]
        [(= 1 n) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(def (square n)
  (* n n))

;;; ------------------------------------------------------------
;; Assertions
;;; ------------------------------------------------------------
(define-simple-check (check-class obj class)
  (is-a? obj class))

;;; ------------------------------------------------------------
;; Display
;;; ------------------------------------------------------------
(def (pdisplay x
               [columns 35]
               [port (current-output-port)])
  (parameterize ([pretty-print-columns columns])
    (pretty-display x port)))

(def (pydisplay . objs)
  (for ([obj objs])
    (display obj)
    (display " "))
  (newline))

;;; ------------------------------------------------------------
;; Generators
;;; ------------------------------------------------------------
(def (gen-get gen num
              [func (lam (x) (pdisplay x) (newline))])
  (match num
    [0              (void)]
    [(? positive?)  (match (gen)
                      ['DONE (void)]
                      [val   (func val)
                             (gen-get gen
                                      (sub1 num)
                                      func)])]
    [_              (error "Invalid number" num)]))

(define (gen->list gen num)
  ;; Returns: a list
  (match num
    [0              null]
    [(? positive?)  (match (gen)
                      ['DONE  null]
                      [val    (cons val
                                    (gen->list gen (sub1 num)))])]
    [_              (error "Invalid number" num)]))

;; The impersonator pattern: yield all
;; values that `gen` yields.
(define-syntax-rule (gen-impersonate gen)
  ;; `g` stops the generator from being re-defined every loop
  (let ([g gen])
    (let loop ()
      (match (g)
        ['DONE 'DONE]
        [any
         (begin (yield any)
                (loop))]))))
