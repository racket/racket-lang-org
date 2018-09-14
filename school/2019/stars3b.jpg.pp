#lang racket/base
;; reprovide from rcon/2019
(require racket/file)
(define doc (file->bytes "../../rcon/2019/stars3b.jpg"))
(provide doc)