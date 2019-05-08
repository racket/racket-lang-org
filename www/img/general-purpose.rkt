#lang racket/gui

(require "racket-lang-picture.rkt")

(define constants
  (code 
   (define N 5)
   (define secret (random N))))

(define frame
  (code 
   (define f
     (new frame% [label "?"]))))

(define callback
  (code 
   (define ((check i) btn evt)
     (message-box
      "."
      (if (= i secret) "✓" "✗")))))

(define install-cbs
  (code
(for ([i (in-range N)])
  (new button%
       [parent f]
       [label (~a i)]
       [callback (check i)]))))

(define go
  (code 
   (send f show #t)))

(racket-lang-picture "general-purpose.png"
                     constants
                     frame
                     callback
                     install-cbs
                     go)