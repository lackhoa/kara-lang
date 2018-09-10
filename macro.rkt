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

;; Switch: an even simpler dispatch than `case`
(define-syntax switch
  (syntax-rules (else)
    ;; Else
    [(_ val [else e1 e2 ...]) (begin e1 e2 ...)]
    ;; No cases left
    [(_ val) (error "Switch" "No match found" val)]
    ;; Cases left
    [(_ val [compare e1 e2 ...] rest ...)
     (if (eq? val compare)
         (begin e1 e2 ...)
         (switch val rest ...))]))

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
