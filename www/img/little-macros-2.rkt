#lang racket

(require racket/splicing)

(define-syntax-rule
  (where definition ((define (locally-defined-id x ...) body-id) ...))
  (splicing-letrec ((locally-defined-id (lambda (x ...) body-id)) ...)
    definition))

(where (define is-5-odd (odd? 5))
       ((define (odd? n) (if (= n 1) #t (even? (- n 1))))
        (define (even? n) (if (= n 0) #t (even? (- n 1))))))

(if is-5-odd "five is odd" "five is not odd")