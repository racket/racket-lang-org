
    Title:The 3n+1 problem
    Date:2012-10-24T17:46:00.002-04:00
    Tags:

*posted by Danny Yoo*


1``Introduction: 
I’m starting to go through
[Programming
Challenges: The Programming Contest Training Manual](http://www.amazon.com/exec/obidos/ASIN/0387001638/thealgorithmrepo), by Steven S. Skiena and
Miguel Revilla.  I thought it would be fun to show how to approach the problems
using the [Racket](http://racket-lang.org/) programming language.  Rather
than use a small, toy educational subset of the language, I’ll take off the kid
gloves, and use whatever’s available in
[#lang racket](http://docs.racket-lang.org/guide/index.html).


2``The problem: 



The [3n+1](http://acm.uva.es/p/v1/100.html) problem is as follows:
consider a positive number n.  The cycle length of n is the number of
times we repeat the following, until we reach n=1:




* If n is odd, then n ⇐ 3n+1

* If n is even, then n ⇐ n/2




For example, given n=22, we see the following sequence: 22 11 34 17 52 26 13 40
20 10 5 16 8 4 2 1.  The cycle length of 22 is, therefore, 16, since it took 16
repetitions to get to 1.

Given a definition of cycle length of a number, here’s the rest of the problem:
given any two numbers i and j, compute the maximum cycle length over all
numbers between i and j, inclusive.


2.1``The plan: 
Before we do any real coding, let’s figure out a plan of attack and how to test
that plan.



* We need a way of computing cycle-length.

* We need to run cycle-length over a range of values and pick out the biggest result.

It sounds like we may want a function called cycle-length that will
compute how long it takes for us to get n to 1.  If we have
cycle-length as a helper function, then it becomes a fairly direct
loop through the range between i and j to pick out which one produces the
largest cycle length.

Let’s first write up a stub function that computes some nonsense.  We’ll
correct it in a moment, of course!



;cycle-length: positive-integer -> positive-integer
;Computes the cycle length of n, according to
;the 3n+1 rules.

> (define(cycle-lengthn)
42)








This is certainly not right, but it’s a start.  And it’s something we can test!


3``Test cases: 



We want 



(cycle-length1)==>1


Let’s express this more formally with the rackunit unit testing
library in Racket.



;Load up rackunit:
> (requirerackunit)




;Let's express that test:
> (check-equal?(cycle-length1)1)

--------------------
FAILURE
name: check-equal?
location: (eval 4 0 4 1)
expression: (check-equal? (cycle-length 1) 1)
actual: 42
expected: 1

Check failure
--------------------






;A few more tests, according to the problem statement above:
> (check-equal?(cycle-length2)2)

--------------------
FAILURE
name: check-equal?
location: (eval 5 0 5 1)
expression: (check-equal? (cycle-length 2) 2)
actual: 42
expected: 2

Check failure
--------------------






> (check-equal?(cycle-length4)3)

--------------------
FAILURE
name: check-equal?
location: (eval 6 0 6 1)
expression: (check-equal? (cycle-length 4) 3)
actual: 42
expected: 3

Check failure
--------------------






> (check-equal?(cycle-length5)6)

--------------------
FAILURE
name: check-equal?
location: (eval 7 0 7 1)
expression: (check-equal? (cycle-length 5) 6)
actual: 42
expected: 6

Check failure
--------------------






> (check-equal?(cycle-length22)16)

--------------------
FAILURE
name: check-equal?
location: (eval 8 0 8 1)
expression: (check-equal? (cycle-length 22) 16)
actual: 42
expected: 16

Check failure
--------------------








All of our test cases fail.  Hurrah!


4``A solution: 
Ok, now that we coded up the tests, let’s write a solution.  We can write out a
definition for cycle-length almost straight out of the problem
statement:




> (define(cycle-lengthn)
(cond
[(=n1)
1]
[(odd?n)
(add1(cycle-length(add1(*3n))))]
[(even?n)
(add1(cycle-length(/n2)))]))






;Let us try it out on a few values:
> (cycle-length1)
1
> (cycle-length2)
2
> (cycle-length22)
16


If we run this through our test suite, we should be fairly confident
that cycle-length is probably doing the right thing.
(... modulo crazy inputs into the function such as 0.  If we
want to guard against such inputs, we can use the features in
racket/contract.)


5``Optimizing cycle-length: 
How fast is the performance for cycle-length?  Let’s try timing it for
a few values, using the time utility.  We’ll run cycle-length for a
range of numbers, and see how long it takes.




> (time(for([i(in-range1100000)])
(cycle-lengthi)))


cpu time: 890 real time: 889 gc time: 0







5.1``Introducing an accumulator: 
There are a few things we might do to improve the performance of this.  Having
the (add1 ...) in the definition, waiting until the recursion finishes
up, seems ok, but I’m curious to see whether writing the definition with an
explicit accumulator will help us.




> (define(cycle-lengthn)
(cycle-length/accn1))






;Helper function:

> (define(cycle-length/accnacc)
(cond
[(=n1)
acc]
[(odd?n)
(cycle-length/acc(add1(*3n))(add1acc))]
[(even?n)
(cycle-length/acc(/n2)(add1acc))]))











With this reformulation, how does this do now?





> (time(for([i(in-range1100000)])
(cycle-lengthi)))


cpu time: 790 real time: 790 gc time: 0









This does help.  Although we do get an improvement, let’s drop this
version for now and go back to our previous definition since it’s
simpler—and because the next potential optimization will work better
on it!


5.2``Adding memoization: 
Another thing that comes to mind is this: our first good version of
cycle-length works recursively.  More to the point: repeated use of
cycle-length can reuse prior results that we computed earlier.  Maybe
memoization will help.  Let’s try it out: we’ll keep a small table of
results, and consult that to see if we’ve already encountered the solution
before.



;We'll maintain a table of known results.
> (definetable(make-hash))





> (define(cycle-lengthn)
(cond
;Consult the table:
[(hash-has-key?tablen)
(hash-reftablen)]
[else
;If we can't find it, compute it...
(defineanswer
(cond
[(=n1)
1]
[(odd?n)
(add1(cycle-length(add1(*3n))))]
[(even?n)
(add1(cycle-length(/n2)))]))
;... and then put it into the table.
(hash-set!tablenanswer)
;Don't forget to return the value back!
answer]))








Does the overhead of setting up this table pay for itself?  Let’s see:




> (time(for([i(in-range1100000)])
(cycle-lengthi)))


cpu time: 217 real time: 217 gc time: 44






Hey, not bad at all!  That’s significantly better.

We should make sure, of course, that all our test cases are running on this ok.



> (check-equal?(cycle-length1)1)




> (check-equal?(cycle-length2)2)




> (check-equal?(cycle-length4)3)




> (check-equal?(cycle-length5)6)




> (check-equal?(cycle-length22)16)






All’s quiet on the cycle-length front.  The tests are all passing.


5.3``Advanced: abstracting memoization to a helper macro: 
It turns out that the kind of memoization we’ve done here can be lifted out, so
that we can easily perform it at will.  That is, what we’re doing is something
like the following:



Given a definition that we’d like to memoize:



* create a table for exclusive use by the definition, and

* slightly tweak the definition’s body so it consults the table
before going through computation.




In terms of Racket, we can say that like this:




;A little helper to centralize the memoization logic
;into a single rewrite rule:

> (define-syntax-rule(define/memo(nameid)body...)
(begin
(definetable(make-hash))
(define(nameid)
(cond
[(hash-has-key?tableid)
(hash-reftableid)]
[else
(defineanswer(beginbody...))
(hash-set!tableidanswer)
answer]))))











This defines a small rewrite rule that expresses the idea of memoizing simple,
1-argument function definitions.  Once we have this define/memo, we
can rewrite cycle-length to use it:




> (define/memo(cycle-lengthn)
(cond
[(=n1)
1]
[(odd?n)
(add1(cycle-length(add1(*3n))))]
[(even?n)
(add1(cycle-length(/n2)))]))








which is nice because it’s easy to read.


6``Cycling back to a loop: 
Now that we have a fairly robust cycle-length function, we can do the
rest of the problem.  Given a range of numbers, we want to go through them,
compute the cycle lengths, and pick out the biggest one.

We can try to write this directly with a for/list to create a list of
all the cycle-lengths, and apply the max across that list.
Let’s write this in code:




> (define(max-cycle-length-rangeij)
(applymax
(for/list([n(in-rangei(add1j))])
;(add1 j) for inclusion ...
(cycle-lengthi))))











Let’s write a few test cases to make sure that this is computing the right
thing:




;From the "Sample Output" section of
;http://acm.uva.es/p/v1/100.html
> (check-equal?(max-cycle-length-range110)20)

--------------------
FAILURE
name: check-equal?
location: (eval 31 0 31 1)
expression: (check-equal? (max-cycle-length-range 1 10) 20)
actual: 1
expected: 20

Check failure
--------------------











What?!  Oh, whoops, I wasn’t using the n in the loop.  Silly me.  Let’s fix that.




> (define(max-cycle-length-rangeij)
(applymax
(for/list([n(in-rangei(add1j))])
(cycle-lengthn))))








Thank goodness for test cases.

Ok, let’s try that again.



> (check-equal?(max-cycle-length-range110)20)




> (check-equal?(max-cycle-length-range100200)125)




> (check-equal?(max-cycle-length-range201210)89)




> (check-equal?(max-cycle-length-range9001000)174)






All passing?  Much better!


6.1``Advanced: maximizing a loop: 
It would be nice if we could directly express taking the maximum
across a for loop.  We’re performing the maximum computation
by first constructing a list of all the cycle lengths, then applying
max over the whole list.  Can we avoid that auxiliary list
construction, and just compute the max as we’re running through the
numbers?

In fact, there are several variations of for loops in Racket,
so maybe one of those variations will work for us.  For example, we
could use for/fold, which gives us enough expressive power to
take the maximum during iteration.




> (for/fold([current-max-inf.0])
([n'(31415926)])
(if(>ncurrent-max)ncurrent-max))


9


There are other versions of for loops, such as the one for
taking sums (for/sum).  But as of this writing, there doesn’t
seem to be be a for/max form that lets us take the maximum
directly.

The question arises: how difficult is it to build for/max?

It turns out that it’s not too bad, though it requires a little more macrology:
we’ll use for/fold/derived to express our own for/max loop in terms of folding:




> (define-syntax(for/maxstx)
(syntax-casestx()
[(_clauses. defs+exprs)
(with-syntax([originalstx])
#'(for/fold/derivedoriginal
([current-max-inf.0])
clauses
(definemaybe-new-max
(let(). defs+exprs))
(if(>maybe-new-maxcurrent-max)
maybe-new-max
current-max)))]))








Essentially, as we’re looping through numbers, we maintain a
current-max, and update that max accordingly as we walk
across the iteration.  The rest of the code in for/max
delegates the rest of the gruntwork to
for/fold (technically, for/fold/derived).

We must test this, of course:



;Edge case: if we take the maximum of no numbers,
;let's see -inf.0.

> (check-equal?(for/max([i'()])
i)
-inf.0)







> (check-equal?
(for/max([i'(31415926)])
i)
9)







> (check-equal?
(for/max[(i(in-range123))]
i)
22)







> (check-equal?
(for/max([n'(3.141592.718281.61803)]
[s'(-111)])
(*ns))
2.71828)






;... and of course...

> (check-equal?
(for/max[(i(in-range900(add11000)))]
(cycle-lengthi))
174)








Looks good.  With this, let’s express max-cycle-length-range
in terms of for/max now:




> (define(max-cycle-length-rangeij)
(for/max([n(in-rangei(add1j))])
(cycle-lengthn)))









7``Making a module: 
Now that we have most of the solution worked out, let’s make a module
that encapsulates what we’ve done.  Let’s lift up the definitions that
we used to make the solution nice and pretty, and place them into
"helpers.rkt":







"helpers.rkt"



#langracket

(providefor/maxdefine/memo)

(define-syntax(for/maxstx)
(syntax-casestx()
[(_clauses.defs+exprs)
(with-syntax([originalstx])
#'(for/fold/derivedoriginal
([current-max-inf.0])
clauses
(definemaybe-new-max
(let().defs+exprs))
(if(>maybe-new-maxcurrent-max)
maybe-new-max
current-max)))]))


(define-syntax-rule(define/memo(nameid)body...)
(begin
(definetable(make-hash))
(define(nameid)
(cond
[(hash-has-key?tableid)
(hash-reftableid)]
[else
(defineanswer(beginbody...))
(hash-set!tableidanswer)
answer]))))

(module+test
(requirerackunit)
(check-equal?(for/max([i'()])
i)
-inf.0)
(check-equal?(for/max([i'(31415926)])
i)
9)
(check-equal?(for/max[(i(in-range123))]
i)
22)

(check-equal?
(for/max([n'(3.141592.718281.61803)]
[s'(-111)])
(*ns))
2.71828))





Who knows?  We might reuse "helpers.rkt" sometime.


(You may note that the bottom of "helpers.rkt" contains a
test
submodule which collects the unit tests that we’ve written.  We can
run a module’s test suite by using
[raco test](http://docs.racket-lang.org/raco/test.html).)

With our "helpers.rkt" in in hand, let’s put our solution in
"three-n-plus-one.rkt":




"three-n-plus-one.rkt"



#langracket

(require"helpers.rkt")

(define/memo(cycle-lengthn)
(cond
[(=n1)
1]
[(odd?n)
(add1(cycle-length(add1(*3n))))]
[(even?n)
(add1(cycle-length(/n2)))]))

(define(max-cycle-length-rangeij)
(for/max([n(in-rangei(add1j))])
(cycle-lengthn)))


(module+test
(requirerackunit)

(check-equal?(cycle-length1)1)
(check-equal?(cycle-length2)2)
(check-equal?(cycle-length4)3)
(check-equal?(cycle-length5)6)
(check-equal?(cycle-length22)16)

(check-equal?
(max-cycle-length-range110)20)
(check-equal?
(max-cycle-length-range100200)125)
(check-equal?
(max-cycle-length-range201210)89)
(check-equal?
(max-cycle-length-range9001000)174)
(check-equal?
(for/max[(i(in-range900(add11000)))]
(cycle-lengthi))
174))




8``Integrating with I/O and a main: 
Finally, all this unit testing is fine and dandy, but we don’t
actually read input from standard input.  Let’s fix that, and modify
"three-n-plus-one.rkt" so it can be run as the main
entry point.




We can read individual lines as strings by iterating across
current-input-port with in-lines:




(for([line(in-lines(current-input-port))])...)


Once we have a line in hand, we can parse out the individual chunks
with read.  read doesn’t normally read from strings
directly, so we first translate each string into a port-like value
using open-input-string.

Last of all, let’s add the following to the bottom of
"three-n-plus-one.rkt":



(module+main
(for([line(in-lines(current-input-port))])
(defineline-port(open-input-stringline))
(definei(readline-port))
(definej(readline-port))
(when(and(number?i)(number?j))
(printf"~a ~a ~a\n"
ij
(max-cycle-length-rangeij)))))


This defines a main submodule.  When we run
"three-n-plus-one.rkt" directly from the command line, it will
run main:



$ cat sample-data.txt
1 10
100 200
201 210
900 1000

$ racket three-n-plus-one.rkt < sample-data.txt
1 10 20
100 200 125
201 210 89
900 1000 174



9``The files: 


* [helpers.rkt](http://hashcollision.org/three-n-plus-one/helpers.rkt)

* [three-n-plus-one.rkt](http://hashcollision.org/three-n-plus-one/three-n-plus-one.rkt)



<!-- more -->



* * *

Why (define answer ...) rather than a let?

— *Ben W, 25 October 2012*

* * *

For the short answer, see the Racket Style Guide, Section 4.2.

— *John Clements, 25 October 2012*

* * *

The “3n+1 problem” is also known as the Collatz conjecture.

— *roryokane.com, 25 October 2012*

* * *

Here's a solution in Factor:

http://re-factor.blogspot.com/2012/10/the-3n1-problem.html

— *mrjbq7, 25 October 2012*

* * *

Wonderful post.  Have you considered using hash-ref! to make define/memo a little simpler as in:

(define-syntax-rule (define/memo (name id) body ...)
  (begin 
    (define table (make-hash))
    (define (name id)
      (hash-ref! table id (thunk body ...)))))

— *Marty N., 29 October 2012*

* * *

@marty: I wanted to keep the correspondence between the non-macro version of the function and the macro as clear as possible.  That way, it's a little more clear how the code evolves.

— *Danny Yoo, 1 November 2012*

* * *

