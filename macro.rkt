#lang racket
(provide (all-defined-out))


;; "lam" instead of "lambda"
(define-syntax-rule (lam whatever ...)
  (lambda whatever ...))

;; "def" instead of "define"
(define-syntax-rule (def whatever ...)
  (define whatever ...))

;; "def*" instead of "define-values"
(define-syntax-rule (def* (id ids ...) expr)
  (match-define (list id ids ...) expr))

(define-syntax-rule (cons! item ls)
  ;; Add an element to the beginning of the list.
  (set! ls (cons item
                 ls)))

(define-syntax-rule (stream-cons! item stream)
  ;; Same, but with streams
  (set! stream (stream-cons item
                            stream)))

(define-syntax-rule (append! mutated-ls ls)
  ;; Same idea, but with append
  (set! mutated-ls
        (append mutated-ls ls)))

(define-syntax-rule (cdr! ls)
  ;; I'm just gonna do it without saying a thing
  (set! ls (cdr ls)))

(define-syntax-rule (set!ret var val)
  ;; Set and then return the value
  (begin (set! var val)
         var))

(define-syntax-rule (assert bool msg objs ...)
  (unless bool
    (error msg objs ...)))
