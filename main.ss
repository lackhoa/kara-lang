;;; Macros


;;; Functional Stuff
(define >>
  ;; Haskell's do notation (or at least what I think it is)
  (lambda (x . fs)
    (if (null? fs)  x
        (and x
           (apply >>
             ((car fs) x) (cdr fs))))))

(define f>
  ;; 'f' stands for 'first'
  (lambda (fun . args)
    (lambda x
      (apply fun (append x args)))))

(define l>
  (lambda (fun . args)
    (lambda x
      (apply fun (append args x)))))

(define f>>
  (lambda args
    (apply f> >> args)))

(define identity
  (lambda (i) i))

(define pass
  (lambda (pred)
    (lambda (x)
      (if (pred x) x #f))))

(define negate
  (lambda (fun)
    (lambda args
      (not (apply fun args)))))

(define flatmap
  (lambda (fun ls)
    (apply append (map fun ls))))

(define capture
  (lambda (x) (begin (pydisplay x)
                x)))

(define repeat
  (lambda (times func!)
    (do ([times times (- times 1)])
        ((<= times 0))
      (func!))))

(define eq*?
  (lambda (arg1 . args)
    (for-all (lambda (x) (eq? x arg1))
       args)))

(define equal*?
  (lambda (arg1 . args)
    (for-all (lambda (x) (equal? x arg1))
       args)))

(define (bool x)
  (case x [#f  #f] [else  #t]))

;;; Testing Functions
(define (fib n)
  (cond [(= 0 n) 1]
        [(= 1 n) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define square
  (lambda (n) (* n n)))

;;; Display
(define (pydisplay . objs)
  (for-each (lambda (obj) (display obj)
                    (display " "))
            objs)
  (newline))

(define string-append-spc
  (lambda strings
    (if (null? (cdr strings))  (car strings)
        (string-append (car strings) " "
                       (apply string-append-spc
                         (cdr strings))))))

;;; Sequence
(define filter-false
  (l> filter (lambda (x) x)))

(define (zip l1 . ls)
  (apply map list l1 ls))

(define (len<= ls n)
  (if (fxnonnegative? n)
      (if (null? ls)  #t
          (cond
           [(= n 0)  #f]
           [else     (len<= (cdr ls) (- n 1))]))
      (error "len<=" "What the heck?" n)))

(define (list-head-safe ls n)
  (if (len<= ls n)
      ls
      (list-head ls n)))

(define list-tail-safe
  (lambda (ls n)
    (if (len<= ls n)
        '()
        (list-tail ls n))))

(define last-index
  (lambda (ls) (- (length ls) 1)))

(define last-item
  (f>> last-pair car))

(define remove-duplicates
  (lambda (ls)
    (let ([recur  (lambda _ (remove-duplicates (cdr ls)))])
      (cond [(null? ls)         '[]]
            [(member (car ls)
                     (cdr ls))  (recur)]
            [else               (cons (car ls) (recur))]))))

(define key-merge
  (l> merge (lambda (x y) (< (caar x) (caar y)))))

(define key-sort
  (l> sort (lambda (x y) (< (car x) (car y)))))

(define get
  (lambda (x s)
    (cond [(assq x s)  ⇒ cdr]
          [else        x])))

;;; Pattern matching by Oleg
;; Very different from Racket match
;; You gotta unquote variables to bind them
(define-syntax pmatch
  (syntax-rules (else guard)
    [(_ (op arg ...) cs ...)            (let ([v  (op arg ...)])
                                          (pmatch v cs ...))]
    [(_ v)                              (if #f #f)]
    [(_ v (else e0 e ...))              (begin e0 e ...)]
    [(_ v (pat (guard g ...) e0 e ...)
        cs ...)                         (let ([fk  (lambda () (pmatch v cs ...))])
                                          (ppat v pat
                                                (if (and g ...)  (begin e0 e ...)
                                                    (fk))
                                                (fk)))]
    [(_ v (pat e0 e ...) cs ...)        (let ([fk  (lambda () (pmatch v cs ...))])
                                          (ppat v pat (begin e0 e ...) (fk)))]))

(define-syntax ppat
  (syntax-rules (quote __ unquote)
    [(_ v __ kt kf)             kt]
    [(_ v () kt kf)             (if (null? v) kt kf)]
    [(_ v (quote lit) kt kf)    (if (equal? v (quote lit))  kt
                                    kf)]
    [(_ v (unquote var) kt kf)  (let ([var v]) kt)]
    [(_ v (x . y) kt kf)        (if (pair? v)  (let ([vx (car v)] [vy (cdr v)])
                                                 (ppat vx x (ppat vy y kt kf) kf))
                                    kf)]
    [(_ v lit kt kf)            (if (equal? v (quote lit))  kt
                                    kf)]))
