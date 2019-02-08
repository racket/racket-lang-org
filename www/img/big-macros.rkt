#lang racket

;; Create players that use the same strategy as player but fail after a certain number of rounds 

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; make-failing-players:

#; (->i ((n (listof steps))
        (#:player-failure    (-> placements/c place/c)
         #:take-turn-failure (-> board? action?))
        ;; exactly one of them is not specified! 
        ;; yields a class that satisfies the player contract:
        player%/c))

;; given a series of steps and functions to simulate methods,
;; create a player class that fails in the player or take-turn
;; part of its games, after 'yeah many' steps 

(define (make-failing-player%
         n*
         #:player-failure    (pf #f)
         #:take-turn-failure (tff #f))
  
  (define n0 (if (number? n*) n* (first n*)))
  (define k0 (if (number? n*) 0  (rest n*)))
  
  (class super%
    (super-new (other "aaaaaxxxx"))

    (inherit-field name strategy)

    (define games-in-series# n0)
    (define steps-in-game# k0)
    (define/augment (other-name oname)
      (set! games-in-series# (- games-in-series# 1))
      (set! strategy (new strategy% [player name][other oname])))

    (define-syntax-rule
      (define/failure (method arg) fail smethod)
      (define/override (method arg)
        (set! steps-in-game# (- steps-in-game# 1))
        (if (and (<= games-in-series# 0) (<= steps-in-game# 0) fail)
            (fail arg)
            (super method arg))))

    (define/failure (placement list-of-places) pf initialization)

    (define/failure (take-turn board) tff take-turn)))

(define-syntax-rule (failing-module n kw f)
  (begin
    (require "../Common/player-interface.rkt")

    (provide
     (contract-out
      (rename
       failing-after-3-for-take-turn%
       player%
       player%/c)))

    ;; -----------------------------------------------------------------------------------------------
    (require "failing.rkt")

    (define failing-after-3-for-take-turn%
      (make-failing-player% n kw f))))

(define strategy% 0)
(define super% 1)
(define cboard 2)