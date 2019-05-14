#lang racket/gui

(require pict plot csv-reading)

(define SIZE 512)

(define frame (new frame% [label "Li'l Plot"] [width SIZE] [height SIZE]))
(define canvas
  (new (class canvas%
         (inherit refresh-now)
         (super-new [parent frame] [paint-callback (λ (cv dc) (draw-pict the-pict dc 0 0))])
         (field [the-pict (blank SIZE SIZE)])
         (define/public (update new-pict)
           (set! the-pict new-pict)
           (refresh-now)))))
(define bar  (new menu-bar% [parent frame]))
(define menu (new menu% [parent bar] [label "File"]))
(define item (new menu-item% [parent menu] [label "Open CSV File"] [shortcut #\o]
                  [shortcut-prefix (get-default-shortcut-prefix)]
                  [callback (λ _
                              (define new-file (get-file "Choose a CSV file ..." frame))
                              (define new-pict (file->plot new-file))
                              (send canvas update new-pict))]))

(define (file->plot p)
  (define cr (make-csv-reader (open-input-file p)))
  (define cr-as-list (csv->list cr))
  (match-define (cons (list* x-lab col-labs) dts) cr-as-list)
  (define pt
    (for/list ([col-lab (in-list col-labs)] [i (in-naturals)])
      (points #:label col-lab #:color "red" #:sym i
              (for/list ([row (in-list dts)])
                (match-define (cons x cols) row)
                (map string->number (list x (list-ref cols i)))))))
  (define y-lab (match col-labs [(list y) y] [_ #f]))
  (plot-pict pt
             #:title (path->string p)
             #:width SIZE    #:height SIZE
             #:x-label x-lab #:y-label y-lab))

(send frame show #t)