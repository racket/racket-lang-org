
    Title:I Write Funny-Lookin' Racket Code: An Alternate Style for Delimiters and Indentation
    Date:2012-09-28T11:32:00.000-04:00
    Tags:

*posted by Carl Eastlund*

A lot of people are quite surprised when they see the Racket code I write. Let's say I needed a function to render hash table values as expressions that would produce the same value.  A "normal" Racketeer might write something like the following.

```racket
(define (hash->expr ht)
  (cons 'hash
        (for/fold ([args '()])
                  ([(k v) (in-hash ht)])
          (cons (list 'quote k)
                (cons (list 'quote v)
                      args)))))
```

There might be a few variances in style, especially depending on whether one has Racket or Emacs set up to indent `for/fold` specially.  Almost no one, however, would come up with the code I write.

```racket
(define (hash->expr ht)
  (cons 'hash
    (for/fold
        {[args '()]}
        {[{k v} (in-hash ht)]}
      (cons (list 'quote k)
        (cons (list 'quote v)
          args)))))
```

The biggest reaction I get is from the presence of `{`curly braces`}`, but those are largely incidental as far as I'm concerned. It's all about the indentation to me.





A while back I found that my `.emacs` file was growing in proportion to my Racket code--all of it I had ever written, in fact.  Every new macro in my code or in the latest Racket version needed a line like: `(put 'for/fold 'scheme-indent-function 2)`This would tell Emacs _more or less_ how I wanted it to indent the given form.  So long as I followed the use patterns Emacs could cope with.  For instance, with `for/fold`, Emacs could cope with both of the "special" arguments on the same line as the macro name, or both on separate lines.  Changing that up got weird results.



Also, function arguments would lead to significant rightward-creep in my indentation.  Adding up the lengths of a list of strings, for instance, might look like this:

```racket
(foldl +
       0
       (map string-length
            (list "There"
                  "are"
                  "thirty-four"
                  "characters"
                  "here.")))
```

This wastes a lot of space on the left, and to me it doesn't do enough for readability to justify it.  I don't need my eyes drawn to `0` and `+` nearly that much.



I discovered a new style of indentation in the {_Little_, _Seasoned_, _Reasoned_} _Schemer_ series of books by Dan Friedman and his many cohorts.  These books always start a new indentation level at a fixed distance in from the previous one, regardless of the cause for the indentation.  Arguments on the same line as the function or macro name are ignored; they do not "push" indentation over to the right at all.



This indentation style has a lot of appeal to me for a number of reasons.  One, it wastes no space on the left.  Two, it never needs to "know" what a given macro means.  It doesn't matter if you're applying `+` or `lambda` or `for/fold`, all lines beyond the first move two (or however many) characters to the right.  I saw a light at the end of the tunnel: no more `.emacs` customization for every new form!



This style leaves two issues.  One, how to indent `cond`?  The _Little_ books treat `cond` differently, indenting each clause only as far as the keyword `cond`, while other form's arguments are pushed in slightly farther than the function or macro name.  Two, how to "fix" forms like `for/fold` where a few lines really ought to be indented differently?  A straight-up interpretation of this style would generate code like this:

```racket
(for/fold
  ([x 0])
  ([str (in-list '("12" "characters"))])
  (define n (string-length str))
  (+ x n))
```

Now we can't tell visually where the `for/fold` iteration clauses leave off and the loop body definitions and expressions begin.



The `cond` issue is easy enough to resolve.  In Racket, unlike in vanilla Scheme, we use `[`brackets`]` around `cond` clauses.  The same goes for a number of other repeated clauses, in fact: `let`, `match`, `syntax-parse`, and so forth.  So I decided my new, custom indentation style would indent `[`brackets`]` differently from `(`parentheses`)`.  Parens indent one farther than brackets.  That way,

```racket
(let ([x 1]
      [y 2])
  (+ x y))`doesn't become

`(let ([x 1]
       [y 2])
  (+ x y))
```

Since I already use `[`brackets`]` every time I have a repeated, non-expression clause, this rule does exactly what I need it to do.



Once I had differentiated `[]` from `()`, resolving the `for/fold` issue was obvious.  I needed a new indentation rule and a new lexical marker: `{`braces`}`.  Now every time I have a _fixed number_ of special non-expression forms in a macro, I wrap them in braces.  Anything in braces is indented slightly farther (four spaces rather than two) than ordinary sub-expressions.  So my `for/fold` example comes out like this.

```racket
(for/fold
    {[x 0]}
    {[str (in-list '("12" "characters"))]}
  (define n (string-length str))
  (+ x n))
```

Suddently it's quite clear which parts are "special" in the `for/fold` macro.



So now I write code using `(`parentheses`)` for definitions, expressions, and anything else resembling a nestable, expandable term (e.g. `match` patterns, syntax templates), `[`brackets`]` for repeated, non-expandable clauses (e.g. `cond` clauses, `let` bindings), and `{`braces`}` for non-repeated, non-expandable forms (e.g. `lambda` formals, _groups_ of `let` bindings).  And I don't bother to align function arguments; I tend to treat the most significant argument as an "accumulator", and put everything else on one line if I can.

```racket
(foldl + 0
  (map string-length
    (list
      "There"
      "are"
      "thirty-four"
      "characters"
      "here.")))
```

The way I read this code, the first line tells us we are performing a summation; the second line tells us we want the length of each string; the third line tells us we have a list coming; and the rest give its contents.  The result "accumulates" from a list to its lengths to their sum as the indentation cascades out and up from the inside.



With these three rules, I now write my Racket code without bothering to customize my `.emacs` file as I go.  I just use delimiters judiciously to tell Emacs how I want everything indented, and everything comes out pretty much how I want it.



For anyone interested in installing this indentation mode or looking at its source code, I've put the file up on GitHub at:

[https://github.com/carl-eastlund/simple-sexp](https://github.com/carl-eastlund/simple-sexp)

To use it, just put it somewhere your Emacs knows to look for Elisp code and add `(require 'simple-sexp)` to your `.emacs` file.



**Addendum:** Oh, and there's some structured s-expression editing code in that file as well.  It preserves matched parens, brackets, braces, and quotes (for strings).  It's probably a much inferior implementation of things like paredit; this code represents the flailings of an Elisp novice.  Use at your own peril.



<!-- more -->



* * *

Beautiful!

— *Robby Findler, 28 September 2012*

* * *

There is a good reason for the arguments of a function to be lined up: it makes the sexpr tree easily visible, no thoughts on how things match up needed.  And indeed, as someone who is used to having this very visible structure, I find the two-space indentation for all arguments except for the first that is left on the first line much harder to read.  Combined with multiple expressions on the first line I can see how this can get to be even worse (though not with simple "+" and "0").  (Obviously, you'll disagree as someone who is used to not getting these visual cues, which is why these are fertile flamewar materials.)

In addition, IIUC, the curly brace thing is a way that forces you to specify stuff on each and every form, instead of making your editor know about these forms -- and in that case it looks like a bad deal for me, sacrificing visual readability, and overall requiring me to do more work than adding one more name into my editor configuration.


— *Eli Barzilay, 28 September 2012*

* * *

re:Eli --

Yep, this style definitely isn't for everyone, and it's definitely got some huge tradeoffs.  I am acutely aware that I sometimes sacrifice the ability to easily tell how many arguments something has, and that's one place I'm not sure I made the right decision.  I am careful to only put multiple things on the same line if they're right after the function name, or in the case of functions like error or printf where I'm just filling in values for the "~a" or "~s" escapes.  Otherwise, after the first line, I definitely want a clear visual marker of how many arguments there are.

As for the curly braces, I like that I have to put some thought into the indentation myself.  The braces give more than just whitespace to say what's "special".  Frankly I'm annoyed at how much extra duty () does, and I prefer making an explicit distinction.

But I'm definitely not trying to sway anyone here away from traditional Racket style, just presenting my own odd habits.

— *Carl Eastlund, 28 September 2012*

* * *

