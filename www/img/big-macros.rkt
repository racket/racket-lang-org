#lang racket

(require "racket-lang-picture.rkt")

(define req
  (code
   (require (for-syntax syntax/parse))))

(define bfs
  (code
   (begin-for-syntax
     (define (#%app-relation self . **)
       (define stxes (car **))
       (define leest (syntax->list stxes))
       (define count (length (cdr leest)))
       (unless (= (relation-n self) count)
         (raise-syntax-error #f "..." stxes))
       #`(#,(relation-r self) . #,(cdr leest)))

     (struct relation (n r)
       #:property prop:procedure #%app-relation))))

(define dcr
  (code
   (define-syntax (define-checked-relation stx)
     (syntax-parse stx
       [(_ (r:id a:id ...) x:id)
        (define arity (length (syntax->list #'(a ...))))
        (define R (relation arity #'x))
        #`(define-syntax r #,R)]))))

(define checked
  (code
   (define-checked-relation (>checked a b) >)))

(define go
  (code 
   (>checked 0 1)
   (>checked 1 0)))

(racket-lang-picture "big-macros.png"
                     req
                     bfs
                     dcr
                     checked
                     go)