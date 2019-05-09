#lang racket

(provide
 (rename-out [#%lazy-app #%app])
 (except-out (all-from-out racket) #%app))

;; --- the compiler extension 
(define-syntax-rule
  (#%lazy-app fun arg ...)
  (#%app fun (thunk arg) ...))