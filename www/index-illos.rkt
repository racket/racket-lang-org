#lang racket
(require racket/gui sugar/debug sugar/list racket/runtime-path)
(define current-dc (make-parameter #f))
(define current-format (make-parameter 'gui))
(define current-target (make-parameter #f))

(define hmax 500)
(define vmax 500)

(define (draw-complex-line-relative start delta)
  (let* ([rps (real-part start)][ips (imag-part start)])
    (send (current-dc) draw-line rps ips
          (+ rps (real-part delta)) (+ ips (imag-part delta)))))

(define (draw-complex-line start end)
  (send (current-dc)
        draw-line
        (real-part start) (imag-part start)
        (real-part end) (imag-part end)))

(define (draw-complex-spline start controla controlb end)
  (define p (new dc-path%))
  
  #;(report* (real-part controla) (imag-part controla)
             (real-part controlb) (imag-part controlb))
  (send p move-to (real-part start) (imag-part start))
  (send p curve-to
        (real-part controla) (imag-part controla)
        (real-part controlb) (imag-part controlb)
        (real-part end) (imag-part end))
  (send (current-dc) draw-path p))

(define-runtime-path illo-dir "img/illos/")

(define (start name)
  (when (not (directory-exists? illo-dir))
    (make-directory illo-dir))
  (current-target (make-bitmap hmax vmax))
  (current-dc (if (eq? 'svg (current-format))
                  (new svg-dc% [width hmax]
                       [height vmax]
                       [output (format "~a~a.svg" illo-dir name)] [exists 'replace])
                  (new bitmap-dc% [bitmap (current-target)])))
  (when (eq? 'svg (current-format))
    (send* (current-dc) [start-doc "start"] [start-page]))
  (send (current-dc) set-smoothing 'smoothed)
  (define pen (make-pen #:color "Gray" #:width 1 #:style 'solid #:cap 'projecting #:join 'miter))
  (send (current-dc) set-pen pen)
  (send (current-dc) set-brush (make-brush #:style 'transparent)))


(define (finish)
  (define result (make-object image-snip% (current-target)))
  (if (eq? 'svg (current-format))
      (send* (current-dc) [end-page] [end-doc])
      result))

(define (draw-drracket)
  (start "drracket")
  (define pt (make-rectangular 125 80)) ; x y coords
  (for ([a (in-range 0 (* 100 pi) (exp 1))])
       (draw-complex-line-relative pt (make-polar 500 a)))
  (finish))

(define (draw-scheme)
  (start "scheme")
  (define step 20) ; between ribs
  (let ([top-pt 500-50i][bottom-pt 500+200i])
    (for ([x (in-range -150 500 step)])
         (define controla (make-rectangular x 50))
         (define controlb (make-rectangular x 125))
         (draw-complex-spline top-pt controla controlb bottom-pt)))
  (finish))


(define (draw-batteries)
  (start "batteries")
  (for/fold ([last-pt #f])
            ([t (in-range 0 (+ (* 2 pi) 0.1) .01)])
    (define A 2) ; amp
    (define a 3) ; freq
    (define B 1) ; amp
    (define b 17) ; freq
    (define delta (/ pi 3)) ; freq shift
    
    (define (normalize-x x)
      (* 500 (/ (+ A x) (* 2 A))))
    
    (define (normalize-y y)
      (* 195 (/ (+ B y) (* 2 B))))
    (define next-pt (cons (normalize-x (* A (sin (+ (* a t) delta))))
                          (normalize-y (* B (sin (* b t))))))
    
    (when last-pt
      (send (current-dc) draw-line (car last-pt) (cdr last-pt)
            (car next-pt) (cdr next-pt)))
    next-pt) 
  (finish))



(define (rotate xs how-far) (shift xs how-far #f #t))

(define (draw-hexagon pos side pred)
  (define p (new dc-path%))
  (define hex-pts (map (λ(angle) (make-polar side angle)) (list (* pi 1/6) (* pi 3/6) (* pi 5/6) (* pi 7/6) (* pi 9/6) (* pi 11/6))))
  (for ([pt (in-list hex-pts)]
        [pt2 (in-list (rotate hex-pts 1))]
        [pt3 (in-list (rotate hex-pts 2))]
        [i (in-naturals)]
        #:when (pred i))
       (send p move-to (real-part pos) (imag-part pos))
       (send p line-to (+ (real-part pos) (real-part pt)) (+ (imag-part pos) (imag-part pt)))
       (send p line-to (+ (real-part pos) (real-part pt2)) (+ (imag-part pos) (imag-part pt2)))
       (send p line-to (+ (real-part pos) (real-part pt3)) (+ (imag-part pos) (imag-part pt3))))
  (send (current-dc) draw-path p))

(define (draw-lang-cube origin side)
  (define possibles '(3 4 5))
  (define hex-depth (list-ref possibles (random (length possibles))))
  (define hexs (let ([r (random 2)])
                 (map (λ(i) (+ r i)) (range hex-depth))))
  (for/fold ([last-side-size side])
            ([i (in-list hexs)])
    (draw-hexagon origin last-side-size (if (odd? i) odd? even?))
    (define smallest-side 10)
    (max smallest-side
         (- last-side-size (random smallest-side (max (floor (/ last-side-size 2)) (add1 smallest-side)))))))



(define (draw-lang)
  (start "lang")
  (send (current-dc) set-brush (make-brush #:style 'opaque #:color "white"))
  (define side 57)
  (define origin (+ side -60 side (* side +i)))
  
  (define (hex-right pt)
    (+ pt (* side (sqrt 3))))
  
  (define (hex-down pt)
    (+ pt (/ (* side (sqrt 3)) 2) (* 1.5 +i side)))

  ; top row
  (draw-lang-cube origin side)
  (draw-lang-cube (hex-right origin) side)
  (draw-lang-cube (hex-right (hex-right origin)) side)
  (draw-lang-cube (hex-right (hex-right (hex-right origin))) side)
  (draw-lang-cube (hex-right (hex-right (hex-right (hex-right origin)))) side)
  ; 2nd row
  (draw-lang-cube (hex-down origin) side)
  (draw-lang-cube (hex-down (hex-right origin)) side)
  (draw-lang-cube (hex-down (hex-right (hex-right origin))) side)
  (draw-lang-cube (hex-down (hex-right (hex-right (hex-right origin)))) side)
  ; 3rd row
  (draw-lang-cube (hex-down (hex-down origin)) side)
  (draw-lang-cube (hex-right (hex-down (hex-down origin))) side)
  (draw-lang-cube (hex-right (hex-right (hex-down (hex-down origin)))) side)
  
  (finish))


(define (draw-oss)
  (start "oss")
  (define (draw-branches last-x last-y xmin xmax)
    (cond
      [(> (- xmax xmin) 4)
       (define midpoint (+ xmin (floor (/ (- xmax xmin) 2))))
       (define left-x (+ xmin (floor (/ (- midpoint xmin) 2)) (random (ceiling (/ (- midpoint xmin) 4)))))
       (define right-x (+ midpoint (floor (/ (- xmax midpoint) 2)) (random (ceiling (/ (- xmax midpoint) 4)))))
       (define y-increment 20)
       (define left-y (+ last-y y-increment (- (/ y-increment 4) (random (/ y-increment 2)))))
       (define right-y (+ last-y y-increment (- (/ y-increment 4) (random (/ y-increment 2)))))
       (send (current-dc) draw-line last-x last-y left-x left-y)
       (send (current-dc) draw-line last-x last-y right-x right-y)
       (draw-branches left-x left-y xmin midpoint)
       (draw-branches right-x right-y midpoint xmax)]
      [else
       
       (send (current-dc) draw-line last-x last-y last-x 260)]))
  
  (define width 500)
  (define starting-x (/ width 2))
  (define starting-y 0)
  (draw-branches starting-x starting-y 1 width)
  (finish))

(define (find-int-ranges ints)
  ;; converts list of ints into list of ranges of consecutive integers
  ;; so '(2) becomes '((2 2)), and '(2 3 4) => '((2 4))
  (define-values (last-sublist sublists)
    (for/fold ([current-sublist null]
               [sublists null])
              ([i (in-list (sort (remove-duplicates ints) >))])
      (cond
        [(empty? current-sublist) (values (list i i) sublists)]
        [(= (add1 i) (car current-sublist))
         (values (cons i (cdr current-sublist)) sublists)]
        [else
         (values (list i i) (cons current-sublist sublists))])))
  (cons last-sublist sublists))

(module+ test
  (require rackunit)
  (check-equal? (find-int-ranges '(2 3 4 42 43 -7 -8 -9 -10 -11)) '((-11 -7) (2 4) (42 43)))) 

(define (simplify-trail t)
  ;; break down a dragon trail into horiz & vert line segments.
  ;; vlink = same x coordinate, y differs by one. Log the smaller-valued end only.
  ;; meaning, the actual link goes from (x y) to (x (add1 y))
  (define vlinks (for/list ([end (in-list t)]
                            [start (in-list (cdr t))]
                            #:when (and (= (car start) (car end)) ; xs are the same
                                        (= 1 (abs (- (cadr start) (cadr end)))))) ; ys differ by 1
                           (list (car start) (min (cadr start) (cadr end))))) ; take min of the ys
  
  (define xs (remove-duplicates (map car vlinks)))
  
  (define xsegs
    (for*/list ([x (in-list xs)]
                [y-range (in-list (find-int-ranges (map cadr (filter (λ(pt) (= (car pt) x)) vlinks))))])
               (define first-y (car y-range))
               (define last-y (add1 (cadr y-range)))
               (list (list x first-y) (list x last-y))))
  
  
  (define hlinks (for/list ([end (in-list t)]
                            [start (in-list (cdr t))]
                            #:when (and (= (cadr start) (cadr end))
                                        (= 1 (abs (- (car start) (car end))))))
                           (list (min (car start) (car end)) (cadr start))))
  
  (define ys (remove-duplicates (map cadr hlinks)))
  (define ysegs
    (for*/list ([y (in-list ys)]
                [x-range (in-list (find-int-ranges (map car (filter (λ(pt) (= (cadr pt) y)) hlinks))))])
               (define first-x (car x-range))
               (define last-x (add1 (cadr x-range)))
               (list (list first-x y) (list last-x y))))
  (append xsegs ysegs))

(define (draw-dragon [simplified #t])
  ;; adapted from https://www.rosettacode.org/wiki/Dragon_curve#Racket
  (start "platform")
  
  (define (dragon-turn n)
    (if (> (bitwise-and (arithmetic-shift (bitwise-and n (- n)) 1) n) 0)
        'L
        'R))
  
  (define (rotate heading dir)
    (case (list dir heading)
      [((R N) (L S)) 'E]
      [((R E) (L W)) 'S]
      [((R S) (L N)) 'W]
      [((R W) (L E)) 'N]))
  
  (define (step pos heading)
    (list
     (+  (car pos) (case heading
                     [(E) 1]
                     [(W) -1]
                     [else 0]))
     (+ (cadr pos) (case heading
                     [(N) 1]
                     [(S) -1]
                     [else 0]))))
  
  (define-values (dir trail)
    (for/fold ([dir 'N]
               [trail-acc '((0 0))])
              ([n (in-range 0 1500)])
      (define next-dir (rotate dir (dragon-turn n)))
      (define last-pos (car trail-acc))
      (define next-pos (step last-pos next-dir))      
      (values next-dir (cons next-pos trail-acc))))
  
  (define shift-x 90)
  (define shift-y 380)
  (define scale-x 8)
  (define scale-y 8); was 8
  (cond
    [(not simplified)
     (for ([start (in-list trail)]
           [end (in-list (cdr trail))])
          (send/apply (current-dc) draw-line (map (λ(x) (+ shift-x (* scale-x x))) (list (car start) (cadr start) (car end) (cadr end)))))]
    [else
     (define trail-segment-pairs (simplify-trail trail))
     (for ([segpair (in-list trail-segment-pairs)])
          (define start (car segpair))
          (define end (cadr segpair))
          ;; x and y are swapped to draw curve 90 deg rotated from normal
          (send/apply (current-dc) draw-line (list (+ shift-y (* scale-y (cadr start)))
                                                   (+ shift-x (* scale-x (car start)))
                                                   (+ shift-y (* scale-y (cadr end)))
                                                   (+ shift-x (* scale-x (car end)))
                                                   )))])
  
  (finish))

(module+ main
  (svg))

(define (svg)
  (parameterize ([current-format 'svg])
    (draw-drracket)
    (draw-scheme)
    (draw-batteries)
    (draw-lang)
    (draw-oss)
    (draw-dragon)))

(define generate-svg-illos svg)
(provide generate-svg-illos)