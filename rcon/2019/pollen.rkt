#lang racket/base
(provide (all-defined-out))
(require (for-syntax racket/base racket/syntax) racket/runtime-path)
(provide (for-syntax #%datum))

(define-syntax (ffd/rp stx)
  (syntax-case stx ()
    [(_ fam file kwargs ...)
     (with-syntax ([rp-name (generate-temporary)])
       #'(begin
           (require racket/runtime-path css-tools/font-face)
           (define-runtime-path rp-name (expand-user-path file))
           (font-face-declaration fam rp-name kwargs ...)))]))