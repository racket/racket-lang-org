#lang racket/base
(require pict/code)

(provide show-cify?)

(current-keyword-list (list* "linklet"
                             "compile-linklet"
                             "instantiate-linklet"
                             "make-instance"
                             "define-record-type"
                             "register-property!"
                             "extract-procedure"
                             (current-keyword-list)))

(define show-cify? #f)
