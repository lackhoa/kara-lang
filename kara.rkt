#lang racket
(module old racket
        (provide (prefix-out old: eq?)
                 (prefix-out old: equal?)
                 (prefix-out old: list-ref)))
(require racket/trace
         racket/generator
         rackunit
         'old)
(provide (all-from-out racket/trace)
         (all-from-out racket/generator)
         (all-defined-out))

;;; Macros
(define-syntax-rule (lam whatever ...)
  (lambda whatever ...))

(define-syntax-rule (def whatever ...)
  (define whatever ...))

(define-syntax-rule (def* (id ids ...) expr)
  (match-define (list id ids ...) expr))

(define-syntax-rule (cons! item ls)
  (set! ls (cons item
                 ls)))

(define-syntax-rule (stream-cons! item stream)
  ;; Same, but with streams
  (set! stream (stream-cons item
                            stream)))

(define-syntax-rule (append! mutated-ls ls)
  (set! mutated-ls
    (append mutated-ls ls)))

(define-syntax-rule (cdr! ls)
  (set! ls (cdr ls)))

(define-syntax-rule (assert bool msg objs ...)
  (unless bool (error msg objs ...)))

(define-syntax-rule (orb e ...)
  (bool (or e ...)))

(define-syntax-rule (andb e ...)
  (bool (and e ...)))

(define-syntax-rule (for/andb e ...)
  (bool (for/and e ...)))

(define-syntax-rule (for/orb e ...)
  (bool (for/or e ...)))

(define-syntax >>
  ;; Do notation like monad
  (syntax-rules ()
    [(_ x)            x]
    [(_ x f1 f2 ...)  (and x
                         (>> (f1 x) f2 ...))]))

;;; Functional Stuff
(def ((capture [f  displayln]) x)
  (begin (f x)
         x))

(def (repeat times func)
  (when (> times 0)
    (func)
    (repeat (sub1 times) func)))

(def (eq? . args)
  (for/andb ([x  (cdr args)])
    (old:eq? x (car args))))

(def neq?
  (negate eq?))

(def (equal? . args)
  (for/andb ([x  (cdr args)])
    (old:equal? x (car args))))

(def nequal?
  (negate equal?))

(def not-null?
  (negate null?))

(def (bool x)
  (match x [#f  #f] [_   #t]))

(def (flip fn)
  (lam (x y) (fn y x)))

;;; Hash Tables
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

;;; Testing Functions
(def (fib n)
  (cond [(= 0 n) 1]
        [(= 1 n) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(def (square n)
  (* n n))

;;; Assertions
(define-simple-check (check-class obj class)
  (is-a? obj class))

;;; Display
(def (pdisplay x
               [columns 35]
               [port (current-output-port)])
  (parameterize ([pretty-print-columns columns])
    (pretty-display x port)))

(def (pprint x
             [columns 35]
             [port (current-output-port)])
  (parameterize ([pretty-print-columns columns])
    (pretty-print x port)))

(def (pydisplay . objs)
  (for ([obj objs])
    (display obj)
    (display " "))
  (newline))

;;; Generators
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

;;; Sequence
(define (split-evenly ls n)
  (let-values ([(q r)  (quotient/remainder (length ls)
                                           n)])
    (let loop ([ls   ls]
               [res  '()]
               [i    0])
      (cond [(= i (sub1 n))
             (rcons res ls)]
            [else
             (let-values ([(ls1 ls2)  (split-at ls
                                                (if (< i r)
                                                    (add1 q)
                                                    q))])
               (loop ls2
                     (rcons res ls1)
                     (add1 i)))]))))

(def exclude-false
  (curry remq* '(#f)))

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


(def (stream-member x seq)
  (cond [(stream-empty? seq)   #f]
        [(equal? x (car seq))  seq]
        [else                  (stream-member x (cdr seq))]))

(def last-index
  (compose sub1 length))

(def (stream-remove x s)
  (stream-filter (lam (item)
                   (not (equal? item x)))
                 s))

(define (flatmap func ls)
  (foldr append null (map func ls)))

(def (drop ls pos)
  (let-values ([(left right)  (split-at ls pos)])
    (append left (cdr right))))

(def (rcons ls single-item)
  ;; Add an item to the end of a list
  (append ls (list single-item)))

(def rcdr
  (curryr drop-right 1))

(def (list-ref ls pos)
  (match pos
    [(or (? positive?) 0)  (old:list-ref ls pos)]
    [_                    (old:list-ref (reverse ls)
                                        (sub1 (- pos)))]))

(def last
  (curryr list-ref -1))

;;; The stack
(define stack%
  (class object%
    (define content null)
    (super-new)

    (define/public (push item)
      (set! content (cons item content)))

    (define/public (pop)
      (when (null? content)
        (error "pop!" "Cannot pop an empty stack!" this))
      (let ([popped-item (car content)])
        (set! content (cdr content))
        popped-item))

    (define/public (peek)
      (car content))))
