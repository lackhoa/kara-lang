#lang racket

(require "macro.rkt"
         "list-prims.rkt")
(provide (all-defined-out)
         (all-from-out "list-prims.rkt"))

;; This file focuses on the lazy way of working with sequences
;; The terminology is like this:
;; Stream = Lazy Sequence
;; List = Strict Sequence

;; Test if `x` is in `seq`, but returns the tail when hit
(def (stream-member x seq)
  (cond [(stream-empty? seq)  #f]
        [(equal? x
                 (car seq))   seq]
        [else                 (stream-member x (cdr seq))]))

(def (exists pred seq)
  ;; Returns #f if doesn't exist, and the first witness if exists.
  (cond [(stream-empty? seq)       #f]
        [(pred (stream-first seq)) (stream-first seq)]
        [else
         (exists pred (stream-rest seq))]))

(def (forall? pred seq)
  (exists (negate pred) seq))

(def (last-index ls)
  (sub1 (length ls)))

(def (stream-remove x s)
  (stream-filter (lam (item)
                   (not (equal? item x)))
                 s))

(define (flatmap func ls)
  (foldr append null (map func ls)))

;; `ls`: a list of streams.
(def (stream-interleave ls)
  (match ls
    ['() empty-stream]
    [(list s) s]
    [ls
     (let ([first (car ls)]
           [rest  (cdr ls)])
       (if (stream-empty? first)
           (stream-interleave (cdr ls))
           (stream-cons (stream-first first)
                        (stream-interleave (append1 rest
                                                    (stream-rest first))))))]))

(def (remove-pos ls pos)
  (cond [(= pos 0) (drop ls 1)]
        [(> pos 0) (append (take ls pos)
                           (drop ls (+ pos 1)))]
        [else (error "REMOVE-POS" "Invalid position" pos)]))

;; Add an item to the end of a list
(def (pad ls single-item)
  (append ls (list single-item)))

;; Synonym
(def append1 pad)
