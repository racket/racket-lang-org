
    Title:Your security hole is my fun hack, or: computing factorial in DrScheme with a click-powered loop.
    Date:2007-12-19T17:11:00.000-05:00
    Tags:

*posted by Robby Findler*


One of the many changes in v4.0 is to close a security hole in DrScheme. Specifically, DrScheme v371 lets the program in the definitions window get a hold of the editor containing said program and manipulate it programmatically. There are lots of bad things one might do with this fact, like circumventing DrScheme's protections and cause it to crash, or even spontaneously exit.



But, we can do something even more fun. Put the following program into a DrScheme window (in v371) and set the language to the mzscheme/textual language. Change "input" to whatever number you wish to compute the factorial of and then hit the Run button until your program transforms itself into the final result.


```racket
(define input 10)
(require (lib "mred.ss" "mred") (lib "class.ss"))
(let* ([ed (let-syntax ([m (λ (stx) (with-syntax ([x (syntax-source stx)]) #'x))])
             (m))]
       [mth (regexp-match 
             #rx"^; ([0-9]+) ([0-9]+)" 
             (send ed get-text 0 
                   (send ed paragraph-end-position 0)))]
       [lckd (send ed is-locked?)])
  (send ed begin-edit-sequence)
  (send ed lock #f)
  (if mth
      (let ([n (string->number (list-ref mth 1))]
            [acc (string->number (list-ref mth 2))])
        (send ed delete 0 (send ed paragraph-end-position 0))
        (if (= n 1)
            (begin (send ed delete 0 (send ed paragraph-end-position 0))
                   (send ed insert (format "~a\n#|" acc) 0)
                   (send ed insert "\n|#" (send ed last-position)))
            (begin (send ed delete 0 (send ed paragraph-end-position 0))
                   (send ed insert (format "; ~a ~a" (- n 1) (* n acc)) 0 0))))
      (send ed insert (format "; ~a 1\n" input) 0))
  (send ed lock lckd)
  (send ed end-edit-sequence))
```

<!-- more -->



* * *

Some folks would call that a feature!

— *Griff, 20 December 2007*

* * *

Oh, indeed! Virus authors consider C's lack of array bounds checking a feature, no doubt. :)

— *Robby, 20 December 2007*

* * *

