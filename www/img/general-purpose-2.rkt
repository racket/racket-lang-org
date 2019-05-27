#lang racket/gui

;; represent a CSV file as a multi-colored scatter plot 

;; expected CSV File Format:
;;  label-1, ..., label-n
;;  value-1, ..., value-n
;;  . . . +

(require csv-reading plot pict)

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

(define item
  (new menu-item%
       [parent menu]
       [label "Open CSV File"]
       [shortcut #\o]
       [shortcut-prefix (get-default-shortcut-prefix)]
       [callback
        (λ _
          (define new-file (get-file "Choose a CSV file ..." frame))
          (when new-file
            (define new-pict (file->plot new-file))
            (send canvas update new-pict)))]))

(define (file->plot p)
  (define cr-as-data (make-csv-reader (open-input-file p)))
  (define cr-as-list (csv->list cr-as-data))
  (match-define (list* (list* x-lab col-labs) dts) cr-as-list)
  (define plot
    (for/list ([col-lab (in-list col-labs)] [i (in-naturals)])
      (points #:label col-lab #:color i #;c #:sym i
              (for/list ([row (in-list dts)])
                (match-define (cons x cols) row)
                (map string->number (list x (list-ref cols i)))))))
  (define y-lab (match col-labs [(list y) y] [_ #f]))
  (plot-pict plot
             #:title (path->string p)
             #:width SIZE    #:height SIZE
             #:x-label x-lab #:y-label y-lab))

(send canvas update (file->plot (build-path "general-purpose-2a.csv")))

(send frame show #t)
