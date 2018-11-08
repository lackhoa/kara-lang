(library (kara-lang main)
  (export assert >> f> l> capture repeat eq*? repeat
          equal*? bool fib square pydisplay
          zip len<= list-head-safe list-tail-safe last-index
          string-append-spc flatmap negate f>> filter-false
          pass identity)
  (import (chezscheme))


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
      (lambda (x)
        (apply fun `(,x ,@args)))))

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
    (lambda (ls) (- (length ls) 1))))

(define (strip-duplicates ls)
  (let loop ((rest ls)
             (so-far '()))
    (if (null? rest)
        so-far
        (loop (cdr rest)
              (let ((first (car rest)))
                (if (member first (cdr rest))
                    so-far
                    (cons first so-far)))))))
