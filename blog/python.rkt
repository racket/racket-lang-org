#lang racket/base

(provide python-executable)

(define python-executable
  (or (find-executable-path "python")
      (find-executable-path "python3")
      (error 'python "cannot find python executable")))
