(library (kara-lang main)
  (export assert >> f> l> capture repeat eq*? repeat
          equal*? bool fib square pydisplay list-<
          zip len<= list-head-safe list-tail-safe last-index
          string-append-spc nat-< flatmap negate f>>)
  (import (chezscheme))


;;; Macros
  (define-syntax list-<
    ;; List dispatch
    (syntax-rules ()
      [(_ ls vnull fnorm)
       (if (null? ls) vnull
           (fnorm (car ls)
                  (cdr ls)))]))

  (define-syntax nat-<
    (syntax-rules ()
      [(_ n zero-val sub1-func)
       (if (zero? n) zero-val
           (sub1-func (- n 1)))]))

;;; Functional Stuff
  (define >>
    ;; Haskell's do notation (or at least what I think it is)
    (lambda (x . fs)
      (list-< fs x
              (lambda (f1 frest)
                (and x
                   (apply >> (f1 x) frest))))))

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
      (list-< (cdr strings)
              (car strings)
              (lambda (x y)
                (string-append (car strings) " "
                               (apply string-append-spc (cdr strings)))))))

;;; Sequence


  (define (zip l1 . ls)
    (apply map list l1 ls))

  (define (len<= ls n)
    (if (fxnonnegative? n)
        (list-< ls
                #t
                (lambda (_ rls)
                  (cond
                   [(= n 0)  #f]
                   [else     (len<= rls (- n 1))])))
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
