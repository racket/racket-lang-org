
    Title:How many occurrences of car in the PLT source code?
    Date:2007-09-06T10:30:00.000-04:00
    Tags:

*posted by Robby Findler*

Lets play a guessing game. See who can guess:




*  How many occurrences of the identifier `car` there are in the PLT tree (when using `read` and just counting the symbols that come out)? 

*  Where does `car` rank on the list of the most commonly used identifiers?


*  What is the most common identifier, and how many occurrences of it are there?

UPDATE: The two files `raw-hattori` and `raw-kajitani.ss` are generated files containing solutions to [Paint by Numbers](http://en.wikipedia.org/wiki/Nonogram) problems and about 30,000 occurrences of `x` and `o`. Discounting them, this is the list of the top ten identifiers and the number of occurrences:

```racket
((define 25294)
 (quote 24101)
 (lambda 18883)
 (let 14796)
 (send 14349)
 (x 11877)
 (if 11118)
 (... 8474)
 (car 7610)
 (syntax 6537))
```

The identifier `cdr` ranks 21st with 5,259 occurrences, `let*` has 3,066 which, when combined with `let` comes out at 17,862, still not enough to pass `lambda`. Speaking of combining, `λ` has 2,271 occurrences, which is also not enough to move `lambda`. Finally `map` comes in 32nd with 3,853 occurrences and `foldl` beats out `foldr` (1168th place with 75 occurrences vs 1451st place with 58 occurrences).

<!-- more -->



* * *

I remember reading in Lisp In Small Pieces that CDR is statistically more often encountered that CAR... So my final answer is "less CARs than CDRs in the source code of PLT"

— *Adrien, 6 September 2007*

* * *

That is a good guess, but it turns out not to be the case. Maybe we just write code where we say (car l) several times in the loop body but only recur in one place, so there's only one (cdr l) but multiple (car l)s. I know some would call that blasphemous programming ...

— *Robby, 6 September 2007*

* * *

Ok, I'll bite...

My first thought was: probably some generic variable name, like x. But when I look at a few source fies of my own, I don't see a lot of xs.

I do see a lot more quotes than I expected, though. So, I'll guess quote, though I think it's not a very good guess.

I wouldn't think that car shows up much at all, actually. Maybe a few hundred times?

— *Matthew Flatt, 6 September 2007*

* * *

quote is not a bad guess. Its up there. x isn't bad either. Neither are at the very top, tho.

— *Robby, 6 September 2007*

* * *

Urr... "let"? Unfortunately, the "let"s are probably going to be diluted by the presence of "let*". I would probably cast my vote for "list" or maybe even... "map"?  No, no, probably not.  Okay, my best (bad) guess is "list".

— *John Clements, 6 September 2007*

* * *

let is pretty high but, surprisingly enough, if you add let*'s numbers to let, then let does not move in the list! list is a bit lower. Still fairly frequent, but not up there with let or quote. map is even lower than list.

— *Robby, 6 September 2007*

* * *

My guess would be for `define' and `quote'.  But when I run a count I get `set!' in the first place.

— *Eli, 6 September 2007*

* * *

Yes, that was a joke.  But my count must be different because I got `x' in the lead.  (With a good gap behind it.)

— *Eli, 6 September 2007*

* * *

Cheater!

Guesses only, please!

— *Robby, 6 September 2007*

* * *

My (pre-counting) guess was `define' or `quote'.  My count showed `x' which is Matthew's guess.

— *Eli, 6 September 2007*

* * *

Okay, we have a winner, then! define tops the list and quote is next.

I'll not reveal the actual numbers yet in case someone actually wants to try to guess them or to guess where car shows up.

— *Robby, 6 September 2007*

* * *

Ok, I'll guess the car is quite far down the list.  Maybe 1% of identifiers, or so.  I think with map/fold/etc. and pattern matching car is used quite infrequently.

— *Noel, 7 September 2007*

* * *

I hadn't thought to count percentages. You're 1/2 right .... the top of the list is already in the 2% range! Indeed, once you're past define and quote, everything is 1.x percent or less.

car is well above map and none of the folds crack the top 100.

— *Robby, 7 September 2007*

* * *

Haaa!
I lost because of blasphemous programming! :D

— *Adrien, 9 September 2007*

* * *

:)

You might want to try to verify that hypothesis -- see if there are multiple (car l)s on the same identifier. I'm sure one could write a script to count such things...

— *Robby, 9 September 2007*

* * *

I presume that the count for quote includes '; I can hardly imagine writing quote as an identifier any more, except perhaps in the context 'quote.

In addition, it's not surprising that foldl beats foldr in an eager language.

— *John Cowan, 26 January 2008*

* * *

Yes, that's right quote refers to uses to ' as we all (quote ...)

— *Robby, 26 January 2008*

* * *

