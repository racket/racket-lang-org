
    Title:control, resumed
    Date:2007-07-30T11:55:00.000-04:00
    Tags:

*posted by matthias*

Since at least some people helped me re-re-invent prompt after my last post, I thought I would remind people that PLT Scheme is the _only_ production system in the world that provides delimited and (truly) composable continuations directly (and w/o loss of TCO properties). So here is the same fragment again: 

```racket
(require (lib "control.ss"))

(define (generate-one-element-at-a-time a-list)
  (define (control-state)
    (for-each (lambda (an-element-from-a-list)
  (control resume-here
    (set! control-state resume-here)
    an-element-from-a-list))
              a-list)
    'you-fell-off-the-end-off-the-list)
  (define (generator) (prompt (control-state)))
  generator)
```

Take a look, compare and contrast with the previous post. Time permitting, I will continue to show you another control poem soon. P.S. See [Adding Delimited and Composable Control to a Production Programming Environment ](http://www.ccs.neu.edu/scheme/pubs/#icfp07-fyf) for details.