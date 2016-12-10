
    Title:Getting rid of `set-car!` and `set-cdr!`
    Date:2007-11-12T07:53:00.000-05:00
    Tags:

*posted by Matthew Flatt*

Functional is Beautiful
---

Scheme is a “mostly functional” language. Although Schemers don’t hesitate to use `set!` when mutation solves a problem best, Scheme programmers prefer to think functionally. Purely functional programs are easier to test, they make better and more reliable APIs, and our environments, compilers, and run-time systems take advantage of functional style.

A Schemer’s functional bias is especially strong when writing programs that process and produce lists. The `map` function, which does both, is a thing of beauty:

```racket
  (define (map f l)
   (cond
     [(null? l) '()]
     [else (cons (f (car l)) (map f (cdr l)))]))
```

The `map` function is most beautiful when the given `f` is functional. If `f` has side-effects, the the above implementation over-specifies `map`, which is traditionally allowed to process the list in any order that it wants (though PLT Scheme guarantees left-to-right order, as above). Arguably, when some other Schemer provides a non-functional `f`, then it’s their problem; they have to deal with the consequences (which may well be minor compared to some benefits of using mutation).

The `map` function might also receive a non-list, but the `map` implementor can guard against such misuse of `map` by wrapping it with a check,

```racket
  (define (checked-map f l)
    (if (list? l)
        (map f l)
        (error 'map "not a list")))
```

and then exporting `checked-map` instead of the raw `map`. This kind of checking gives nicer error messages, and it helps hide implementation details of `map`. We could further also imagine that the raw `map` is compiled without run-time checks on `car` and `cdr`.


The Problem with Mutable Pairs
---

What if someone calls `checked-map` like this?:

```racket
  (define l (list 1 2 3 4 5))
  (checked-map (lambda (x)
                 (set-cdr! (cddr l) 5))
               l)
```

The `f` provided to `map` in this case is not purely functional. Moreover, it uses mutation in a particularly unfortunate way: the ` list?` test in `checked-map` succeeds, because the argument is initially a list, and the mutation is ultimately discovered by a call to `cdr` --- but only if checks haven't been disabled.

If you’re a Schemer, then unless you’ve seen this before, or unless you thought a bit about the title of this section, then you probably didn't think of the above test case for `map`. A Schemer’s view of lists is so deeply functional that it's hard to make this particular leap.

Furthermore, this example is not contrived. If you have either Chez Scheme version 6.1 or a pre-200 MzScheme sitting around, calling `map` as above leads to a seg fault or an invalid memory access:

```bash
  Chez Scheme Version 6.1
  Copyright (c) 1998 Cadence Research Systems

  > (define l (list 1 2 3 4 5))
  > (map (lambda (x) (set-cdr! (cddr l) 5)) l)

  Error: invalid memory reference.
  Some debugging context may have been lost.
```

The `map` example illustrates how mutable pairs can break a Schemer’s natural and ingrained model of programming. Of course, if optimizing and providing friendly error messages for `map` were the only issues with mutable pairs, then it wouldn’t matter; Scheme implementors are smart enough to (eventually) get this right. Unfortunately, the underlying problem is more pervasive.

In the API for a typical Scheme library, lists can be used for many kinds of input and output. Flags for options might be provided in a list. A function might provide information about the current configuration (e.g., the current items in a GUI list box) in a list. Procedures or methods that deal gracefully with list mutation are few and far between. In most cases, the result of unexpected mutation is merely a bad error message; sometimes, however, unexpected mutation of a list can break the library’s internal invariants. In the worst case, the library whose internal invariants are broken plays some role in a system’s overall security.

Mutable lists also interfere with the language’s extensibility. The PLT Scheme contract system, for example, offers a way to wrap an exported function with a contract that constrains its input and outputs, which are optionally (in principle) enforced by run-time checks. Higher-order contracts, such as “a list of functions that consume and produce numbers”, require wrappers on sub-pieces, and these wrappers can be installed only by copying the enclosing list. Copying a mutable list changes the semantics of a program, however, whereas contracts are supposed to enforce invariants without otherwise changing the program. Copying an immutable list creates no such problem.

Finally, mutable lists make the language’s specification messy. The R6RS editors spent considerable energy trying to pin down the exception-raising guarantees of `map`; the possibility of mutable pairs made it difficult to provide much of a guarantee. The standard says that implementations should check that the lists provided to` map` are the same length, but it’s not worth much to require that check, since an argument’s length as a list can change via mutation to the list’s pairs.

Switching to Immutable Pairs
---

The designers of PLT Scheme long ago recognized the problems of mutable pairs, and we introduced functions like `cons-immutable` and` list-immutable` to support programming with immutable lists.  These additions solved some problems --- but only in the cases where we were careful to use immutable lists. The R6RS editors also recognized the problems of mutable pairs, so that `set-car!` and `set-cdr!` were banished to their own library --- but programmers are still free to use that library.

While these are worthwhile steps for many reasons, they do not solve the underlying problem. Library implementors who deal in lists must still either set up elaborate guards against mutation, pretend that the problem doesn't matter, or require the use of a special immutable-list datatype that is incompatible with libraries whose authors set up elaborate guards or ignore the problem.

Why all this hassle? If most Scheme code really does use and expect pairs in a functional way, can't we just switch to immutable pair? Most Scheme code will still work, untold security holes will have been closed, specifications will become instantly tighter, and language extensions like contracts will work better.

Schemers have been reluctant to make this leap, because it has never been clear just how much code relies on mutable pairs. We don’t know how much the switch will cost in porting time and long-term incompatibility, and we don’t really know how much we will gain. We won't know until we try it.

For PLT Scheme v4.0, we’re going to try it. In our main dialects of Scheme (such as the `mzscheme` language), `cons` will create immutable pairs, and `pair?` and `list?` will recognize only immutable pairs and lists. The `set-car!` and `set-cdr` procedures will not exist. A new set of procedure `mcons`, `mcar`, `mcdr`, `set-mcar!`, and `set-mcdr!` will support mutable pairs. (A related v4.0 change is that ` define-struct` by default creates immutable structure types.)

Of course, PLT Scheme v4.0 will support an R5RS language where `cons` is `mcons`, and so on, so many old programs can still run easily in the new version. The difference is that interoperability between R5RS libraries and PLT Scheme libraries will be less direct than before.

Experience So Far
---

PLT Scheme v3.99.0.2 exists already in a branch of our SVN repository, and it will soon move to the SVN trunk. That is, we have already ported at least a half million lines of Scheme code to a dialect without `set-car!` and `set-cdr!`.

The conversion took about eight hours. Obviously, relatively little code had to change. The following are the typical porting scenarios:


* The `reverse!` and `append!` functions were frequently used for “linear updates” by performance-conscious implementors. As our underlying Scheme implementation has improved, however, the performance benefits of these functions has become less. All uses could be replaced with `reverse` and `append`.


* The `set-cdr!` operation was often used to implement an internal queue. Such internal queues were easily changed to use `mcons`,` mcar`, `mcdr`, and `set-mcdr!`.


* An association-list mapping was sometimes updated with `set-cdr!` when a mapping was present, otherwise the list was extended. Since the extension case was supported, it was easy to just update the list functionally. (The relevant lists were short; if the lists were long, the right change would be to use a hash table instead of a list.)


*  A pair was sometime used for an updatable mapping where a distinct structure type is better. The quick solution was to throw in a mutable box in place of the value.


The PLT Scheme code might be better positioned for the switch than arbitrary Scheme code. Most of it was written by a handful of people who understood the problems of mutable pairs, and who might therefore shy away from them. However, the PLT Scheme code base includes a lot of code that was not written specifically for PLT Scheme, including Slatex, Tex2page, and many SRFI reference implementations. With the exception of SRFI-9, which generalizes `set!` to work with pairs, the SRFI implementations were remarkably trouble free. (Thanks to Olin Shivers for making mutation optional in the “linear update” functions like `reverse!` from SRFIs 1 and 32.)

In addition, we looked at a number of standard Scheme benchmarks, which can be found here:

[   http://svn.plt-scheme.org/plt/trunk/collects/tests/mzscheme/benchmarks/common/](http://svn.plt-scheme.org/plt/trunk/collects/tests/mzscheme/benchmarks/common/)

Of the 28 benchmarks, eight of them mutate pairs. Four of those are trivially converted to functional programs, along the lines of the scenarios above. One, `destruct`, is designed specifically to test mutation performance, so it makes no sense to port. Another, `sort1`, is a sorting algorithm that inherently relies on mutation; a functional sort is obviously possible, but that would be a different benchmark. The `conform` benchmark uses mutable pairs for tables in a relatively non-local way; as a modern Scheme program, it would probably be written
with structures, but it’s not trivial to port. The `peval` benchmark uses pairs to represent Scheme programs, and it partially evaluates the program by mutating it, so it is not trivial to port. To summarize, out of 28 old, traditional benchmark programs, only two represent interesting programs that are not easily adapted to immutable pairs. (They run in PLT Scheme’s R5RS language, of course.)


Finally, we selected a useful third-party library that is not included with PLT Scheme. We checked the generic SSAX implementation (not the PLT Scheme version), and we found a couple of uses of `set-car!` and `set-cdr!`. Again, they fall into the above queue and association-list categories that are easily and locally converted.

Meanwhile, as we start to use v3.99 to run scripts in our day-to-day work, immutable pairs have so far created no difficulty at all.  So far, then, our optimism in trying immutable pairs seems to be justified; it just might work.

But It’s Lisp Tradition!
---

A typical response to news of the demise of mutable pairs is that it will create lot of trouble, because mutable pairs are Scheme tradition, and surely lots of useful old code exploits them in lots of places.

We’re eager to hear whether anyone has such code. Our initial hypothesis is that practically all old code falls into one of two categories:


*  The code is easily ported to immutable pairs, along the same lines as above (i.e., local queues and small association lists).


*  The code so old and generic that it can be run as an R5RS program.  It won’t call into the large PLT Scheme set of libraries that will expect immutable pairs, and it can easily be used as a library with wrappers that convert mutable pairs back and forth with immutable pairs.


Frankly, we’re not so eager to hear opinions based on guesswork about existing code and how it might get used. Download v3.99 from SVN or as a nightly build when it becomes available; let us know your guesses about how running your old code would go, but then let us know what actually happens.

The immutable-pairs plan for v4.0 is not set in stone, but we won’t make the decision based on guesswork. More libraries (other than R5RS) to aid compatibility may be useful, but so far we don’t have a tangible need for them. In any case, we’ll revert to mutable pairs only if significant experience with the pre-release version demonstrates that it really won’t work.

<!-- more -->



* * *

A brave and commendable experiment! 

I think there are two distinct issues here: set-cdr! can mutate list structure (and creates problems for functions that assume that list structure is invariant) whereas set-car! mutates list contents  but not structure (and prevents copying). I think the former is much more troublesome than the latter. Did you consider semi-mutable pairs, that is, pairs whose car is mutable but cdr is immutable? In the code you have converted, is set-cdr! more common than set-car!?

— *Alan Watson, 12 November 2007*

* * *

`set-car!` doesn't merely prevent copying (which is a big enough problem for the contract system); it can also break invariants, as in the example of reflecting a list-box's content.

So, keeping `set-car!` leaves us with problems. Would keeping it solve any problems in practice? I did indeed see `set-cdr!` more than `set-car!`, and I can't think of an example that would have run if only we had kept `set-car!`. Thus, aside from tradition, I don't yet see any rationale for keeping `set-car!` --- but I'm eager to hear about other experience.

— *Matthew Flatt, 12 November 2007*

* * *

The only situations where I find myself mutating pairs are, on first glance, generating lists front to back, which you discussed, using pairs to implement simple 'pairs of values', where structs/objects/vectors could be used instead, and for sorted insertion into lists, which is more fundamental, but I guess would be a good place to use mutable pairs.

— *Marijn, 13 November 2007*

* * *

Perhaps all I am saying is that the arguments against set-car! are the standard arguments against mutation, whereas the arguments against set-cdr! are those plus the problems it creates for procedures like map. Thus, the justification for eliminating set-car! is similar to the justification for making records immutable (as you have done), whereas the justification for eliminating set-cdr! is this and more.

— *Alan Watson, 13 November 2007*

* * *

Alan: I think that the arguments against set-cdr! are in fact the same arguments against any old mutation (ie, just like the anti-set-car! arguments). Even tho the core libraries of Scheme (things like map) suffer more due to set-cdr!, ordinary programs suffer equally in our experience (as Matthew has explained).

Anyways, let me encourage you to put your theory to the test and find some non-trivial programs or libraries that back up your point!

— *Robby, 13 November 2007*

* * *

My point is not that there are libraries or programs in which set-car! is a good idea. My point is an opinion, perhaps badly expressed, on why each creates problems and which is worse. Sorry for the noise.

— *Alan Watson, 13 November 2007*

* * *

I tend to agree.  I do use set-car! and set-cdr! time to time, but mostly from my laziness to use more appropriate data structures instead.  Recently I also started to avoid the use of set-cdr! to create list from front to back, for it doesn't work well with call/cc.

However, there's one more area that I see the mutable pairs are indispensable; circular lists.  Though it has its own problems, it is sometimes very handy to express some kind of code concisely where I'd use infinite lazy list in Haskell.  It only needs mutation at construction time, and we can make it "freeze" so that it won't have the problem of "the list changing during traversal".

Do you have some plan to support circular structures specially (e.g. srfi-38 notation or srfi-1's circular-list procedure hides mutation "under the hood" so that the users only see immutable, already constructed structures)?

— *Shiro Kawai, 13 November 2007*

* * *

Yes. Version 3.99.0.2 has `make-reader-graph` for creating cycles built from immutable pairs. We need a better name, but the current name reflects that `read` still supports the #n= and #n# graph notation. (We've dropped support for cyclic syntax objects, though, so `read-syntax` no longer supports the graph notation).

— *Matthew Flatt, 13 November 2007*

* * *

Well, I've been online now for over 30 minutes, and AFAIK, I haven't been hacked.  This is a banner day.  Hi Matt; great article!  Out of necessity, from a security standpoint, I have converted all my boxes to some flavor of *nix.  As soon as I can download 3.99, I'll try to reproduce the flavor of the data from my flatten benchmark article.  This time it will be different, simply because they'll be on a new OS.  However, I have a bunch of varients of the flatten algorithm, using both muttable and immutable methods.  As I recall from my earlier results, the biggest difference advantage to using mutation came about when a poor algorithm had been choosen to begin with, that required the reversal of the result list.  These quite substantially improved their performance with the use of reverse!   However, the more elegant functional solutions, under favorable conditions, actually out performed even the fastest mutable algorithms.  Not typically, but definitely always in the race.  The success of building functional solutions, which are also efficient is certainly achieved with other functional languages.  I'll have to redesign my queue routine, as you mentioned, but it will be worth the effort.  I'm looking forward to getting a copy on my box as soon as possible.

Take care.

--kylr

— *Kyle Smith, 18 November 2007*

* * *

Did not observe any problem beyond the typical usages noted in the article when I moved SSAX and SXML to R6RS libraries.

Typical use is in implementing "efficient" queues.  I expect an applicative queue such as found in Okasaki would work fine on modern hardware and scheme implementations.

I doubt there is a systemic problem out there.

— *GreyLensman, 18 November 2007*

* * *

How difficult would it be to replace set! with explicit boxes?

— *Ethan Aubin, 21 November 2007*

* * *

Are you considering changing the behaviour of eq? or eqv? on (now immutable) pairs? Currently, two immutable pairs whose cars and cdrs are eq? can only be distinguished by eq? or eqv?. As such, should they really be considered to be distinct or is their apparent distinctiveness simply an artifact of the implementation that should be hidden from the programmer?

— *Alan Watson, 26 November 2007*

* * *

Wow, did I make a mistake.

Background. In PLT Redex, the stepper uses mutable lists to represent the currently visible portion of the reduction graph. When someone clicks the "next" button, it does a set-cdr! to add something to the end of that graph.

The Mistake. When I converted the code to v4, I figured that I should preserve the mutation, so I went in and changed a few set-car!s to set-mcar!s and then changed about 15 or 20 uses of filter, for-each, ormap, list-ref, and friends to versions of those that worked on mutable pairs instead of immutable ones. This took about 2 hours.

Not too bad, you might think. Two hours? Sometimes fixing one little bug can take longer than that, days even. Got away cheaply, you might think ...

What I Should Have Done. Today I decided that I wanted the code to work in both v4 and in earlier versions of PLT Scheme, since the code'll be on PLaneT, after all and not everyone is using v4 yet. Sigh. So now I need to go undo that work and copy the list instead of mutating its tail. So I did that.

5 minutes. Done. Working code.

10 more minutes: removing all of the now useless helper functions that dealt with the mutable-pair versions of filter, for-each, ormap, list-ref. 

Oh well. Lesson learned, I suppose.

— *Robby, 27 November 2007*

* * *

On replacing set! with set-box!: in a sense, this is so easy that the compiler does it automatically. That is, mutable variables are already implemented via boxes. You always know statically whether a variable needs to be boxed and unboxed due to lexical scope plus rules on mutating module-defined identifiers. For essentially the same reason, `set!` doesn't really cause much trouble; programmers usually see `set!`s and know to treat the relevant variable with care --- in contrast with `set-car!`s, which could be in a library that you don't see.

— *Matthew Flatt, 27 November 2007*

* * *

We have so far not changed the interaction of `cons` with `eq?` or `eqv?`. Specifically, `cons`, `list`, etc. are guaranteed to create fresh cons cells, in the sense that `eq?` distinguishes them, even though the cons cells are immutable. I think we may want to remove this guarantee eventually, because it would make some constant-folding optimizations easier and enable hash-consing, but we'll take one step at a time.

— *Matthew Flatt, 27 November 2007*

* * *

A little late, but ...

Once one acknowledges value of circular/recursive list data, it follows that there is likely value in the native runtime creation of such data as may be used for example to express arbitrarily recursively defined hierarchical information; which beyond the trivial, seems seems like a significant loss to the language to push such capability outside the baseline scope of the language by default.

Yes, such capability may be defined and utilized as required although not as elegantly as if it were native; however it seems a shame to seriously consider alienating the previously native capability to dynamically formulate native list data from datum not known at program specification/compile time (i.e. the ability to formulate and manipulate such list datum from information extracted during run-time, as data bases typically are, can not be performed natively without set-car! set-cdr!, nor specified purely functionally); as the loss of such a potentially powerful and useful capability seems
like a poor trade in exchange for the questionable value of default immutable lists, especially as the mutability of function argument can most typically be easily determined within a function, and optimizations applied accordingly, and in the few cases where it can't, so what, presume it may be mutated. (If the concern is limited to map, then simply specify that map's arguments are presumed to not be mutated until terminated, or generally forbidden).

— *pschlie, 15 December 2007*

* * *

The entire purpose of this experiment was to avoid caveats like "seems", "likely" and "potentially". If you have real examples to show, that would be interesting. So far we have far better abstract arguments against mutation than for it, and no concrete experience suggesting it should be brought back.

Also, I should remind you that mcons (and friends) exist, as do mutable structs. These are as "native" as it gets.

— *Robby, 15 December 2007*

* * *

Somewhat more concrete: 

- presume the run-time creation, storage and/or retrieval of arbitrarily recursive list structured data is useful (or please prove otherwise).

  (Personally see no value in presenting any particular program utilizing such capabilities, as the potential usefulness of such data structures representing state transition graphs, or more generally arbitrarily self recursive data representations are self evident; just as I see no value in any example given of how list mutation may yield indeterminate behavior in the absents of the specification of evaluation order; as overall the less flexible and deterministic a language is, the less useful it tends to be. Thereby I view dropping native mutable lists in an effort to improve determinism, in effect an attempt to improve a deficiency by removing utility; as opposed to improving its utility, by removing a deficiency.)

Implying:

- a facility to create/manipulate such data at run-time is required.

  - mcons (and friends) are only rudimentarily facilitates, which exclusively operate on a now severely crippled data type because mutable lists have been alienated from the language (incompatible with car and friends) and thereby incompatible with the majority of all native list functions and thereby now require an entirely redundant and largely absent collection of functions to specified to operate on and utilize them (inclusive of their inability to be evaluated as code):

- a facility to store and retrieve such data at run-time is required.

  - as there is no current reader support to denote mutable lists:

      - (mcons 'a '{b}) -> {a . (b)} ; not {a b} as likely
        (cons  'a '(b)) -> (a b)     ; expected by analogy

      - (define x (mcons '+ (mcons 1 (mcons 2 null)))) ; i.e. {+ 1 2}
        (with-output-to-file "f" (lambda () (print x)) 'text 'replace)
        (with-input-from-file "f" (lambda () (read)))
        => (+ 1 2) ; wrong

      - (define x (mcons 1 null)) (set-mcdr! x x) ; i.e. #0={1 . #0}
        (with-output-to-file "f" (lambda () (print x)) 'text 'replace)
        (with-input-from-file "f" (lambda () (read)))
       => #0=(1 . #0#) ; wrong
      
       [not to mention (define x '#0=(1 . #0#)) isn't accepted anymore]

So overall, scheme absent mutable lists removes an entire class of capabilities which which was never proven to be useless, in fact arguably known to be otherwise, nor given analogous facilities of comparable utility.

Yes, such facilities may be structured from non-native structured data types and a corresponding new set of support functions; just as in most languages; however scheme was previously unique in that such a capability was inherent. (while were at it, maybe list data types and functions should be removed entirely, as after all, they're not strictly necessary as evidenced by most all other languages).

In hind sight, justifying mutable list removal because large classes of programs do not require them, or that the language's specification may be easier in their absents, or because the contract facility implementation isn't capable determining if list data is in fact potentially mutated; hardly seems reasonable. IMHO

— *pschlie, 16 December 2007*

* * *

Paul, this is not what I meant by concrete. No one said (or attempted to prove) that mutable cons cells were useless. Indeed, we still support them, but we just do not use them by default anymore.

The comment "justifying mutable list removal because large classes of programs do not require them" misses half of the problem with mutable lists. The first half of the problem, in fact. Matthew's post clearly explains why mutable lists cause problems in concrete ways and, if you permit me to say so, makes a far more convincing abstract case against mutable pairs than your abstract arguments for them.

But, in any case, these abstract arguments (either way) ultimately need to be backed up with real programs. Let me encourage you once again to provide some to back up your arguments. We have found none in a large body of code. Have you investigated a body of code and found some?

— *Robby, 16 December 2007*

* * *

The problems you describe with mutability remind me of type-theory where each kind of thing is separated into its own type.  Then you can always use the types in conjunction with each other if you need the functionality of both.

For example, if you had the types `list` and `mutable` you could distinguish between an immutable list of immutable elements, an immutable list of mutable elements, a mutable list of immutable elements, and so on, and put this in your "contract system" (whatever that is).

This not only documents the usage of things like mutability, but also allows the compiler (in theory) to check such constraints on inputs and outputs.

When a function like `map` expects an immutable list, it will simply use the `list` type without restricting the mutation of elements.  But in places where that could be harmful, a different type can be used.  The conventional Scheme way of allowing everything to be mutable prevents this separation of distinct properties of values.

I'm not too familiar with how Scheme implements things, but the general way this is done is to box things that are mutable.

— *Jon T, 28 December 2007*

* * *

In another five years or so you'll be where ML was twenty years ago!  Better late than never, I suppose, but, really, what is the point?

— *Existential Type, 25 January 2008*

* * *

"Existential Type": your comment begs the exact same reply with macros, except that five years is probably optimistic, and twenty years is an underestimate.  Or type reflection that maybe some day will get ML some of the advantages of Scheme.

(But this is, of course, a pointless flame, just like your comment.  FWIW, the main issue is legacy code, and switching to Scheme with immutable lists is slightly easier than switching to ML.  At least for us Scheme freaks.)

— *Eli Barzilay, 25 January 2008*

* * *

The conversion of my code took less than a day (about 10000 lines, a guess, for I never counted the nr of lines) There are two types of instances in which I used set-cdr!: for assoclists and imperative queues. Assoclists now have become immutable lists of mutable pairs with a hybrid-assq procedure added. The imperative queues remain mutable lists, the cdr of the last pair being the only one being updated. My code did not contain anything like append! or reverse!. I found no trouble in swithing to v3.99.0.9. I say yes to the better conditions for optimization.
Jos Koot

— *jos koot, 26 January 2008*

* * *

Eli, you seem forget that all Scheme fits into tiny corner of ML, namely a single recursive datatype.

— *Existential Type, 26 January 2008*

* * *

Eli, I agree with you about the pointlessness of an ML-vs-Scheme flame war.  Can you be more explicit about what you mean by "type reflection"?  That sounds interesting, but I have no idea what you're talking about.

— *Michael, 26 January 2008*

* * *

Existential Type: From this side of the fence I can say that Scheme covers all of ML, with all types that were and will be written.  But this is a second pointless flame, and still irrelevant to the context.  I could just as well point at the many limitations of ML types (eg, what's the real type of map? sqrt? printf? collatz? GetFunction("foo","bar.dll")?).  But that's not going to do any good, so I'll stop replying.  Feel free to continue on the PLT list, or email me.

Michael: I'm talking about several efforts that attempt to get the benefits of a dynamic language into a static language like ML.  There was some work done on reflecting types (or reifying types) which makes it possible to sort of package a value together with a type object (in the run-time sense; it becomes a value).  IIRC, there was also something related to universal types that goes in the same direction.

Personally, I found that MLers who blindly bash Scheme for it's lack of types are just as bad as Schemers who blindly bash all static typing.  IMO (and in the opinion of many other good people I've talked to) there are very good advantages on both sides -- and the real challenge is to get dynamic languages like Scheme to benefit from the advantages of statically typed languages like ML, and vice versa.  Some people choose to work on just that (from either side), some people don't but appreciate those who do, and some people will inevitably continue to spit out useless comments even in a completely unrelated context, like the above anonymous poster.

— *Eli Barzilay, 26 January 2008*

* * *

Don't you just love it when language maintainers drop a feature because someone, somewhere, might misuse it?

What's next? Dropping the number zero, because it can cause a divide-by-zero error? 

I'm de-installing PLT today and switching to an implementation which doesn't think I'm an idiot.

— *T., 24 June 2008*

* * *

T: In fact, *most* modern languages
dropped the number zero as a
pointer.  Most languages also
restrict you from using a ton of
other features, so which
implementation are you going to
install: C? assembler? maybe
you'll get a soldering iron?

— *Eli Barzilay, 24 June 2008*

* * *

T: You do realize, don't you, that set-car! and set-cdr! are still available in PLT Scheme 4.0, only as a library instead of as a built-in feature?  Given that, I don't see what you are so upset about.  You're free to use set-car! and set-cdr! as much as you want.

— *Michael Vanier, 24 June 2008*

* * *

I know this is an old, and probably dead thread, but I did want to respond to one post.

----Quote----
Robby said...
...
    But, in any case, these abstract arguments (either way) ultimately need to be backed up with real programs. Let me encourage you once again to provide some to back up your arguments. We have found none in a large body of code. Have you investigated a body of code and found some?
    December 16, 2007 6:56 PM 
----End Quote----

Over 25 years ago, I used set-cdr! in a PC Scheme implementation of N-Queens for an AI class.  Besides using set-cdr!, calls to move a queen were properly tail recursive.  I noticed the PLT Scheme benchmark didn't use tail recursion for moves, so I modified my code to match the benchmark.  The following times compare the improvements for just using set-cdr! and for using set-cdr! combined with tail recursion:

PLT Scheme Benchmark:
> (begin (collect-garbage) (time (nqueens 15)))
cpu time: 158899 real time: 158982 gc time: 16920
2279184
(execution time = cpu time - gc time = 142.0s)

set-cdr! version:
> (begin (collect-garbage) (time (N-Queens 15)))
cpu time: 138616 real time: 138787 gc time: 3286
2279184
(execution time = cpu time - gc time = 135.3s)

set-cdr! plus properly tail recursive moves:
> (begin (collect-garbage) (time (N-Queens 15)))
cpu time: 119845 real time: 120061 gc time: 3284
2279184
(execution time = cpu time - gc time = 116.5s)

As you can see, using set-cdr! improved overall cpu time by about the same amount as using properly tail recursive moves.  The major performance improvement for set-cdr! came from reduced garbage collection.  N-Queens is just one example of back-tracking used to traverse a search space.  Any time the search space can be pruned before the back-tracking call is made, and restored on return from the call, set-cdr! should yield improved performance.  It's also very localized, as shown by the following code snippet:

(else
..(set-mcdr! prior-column (mcdr column))
..(begin0
....(place&move safe-columns
...........(mcdr safe-columns) safe-columns
...........(cons (mcar column) solution) count)
....(set-mcdr! prior-column column))))))

In the above example, prior-column and column traverse the list, while safe-columns always points to it's beginning.  The list is never copied, just manipulated with set-mcdr! as the search space is traversed.  Also, the list goes from 0 to N to simplify handling prior-column.

Gene Snider

— *Gene Snider, 27 January 2010*

* * *

Way to go PLT Scheme 4.x.  You've just made it impossible for me to use your Scheme to work through the exercises in The Scheme Programming Language (4th edition) -- see for example Exercise 2.9.3 on page 55.

As if learning a language like Scheme isn't hard enough for us poor suckers trying to break out of the imperative/object world.  Do you really have to add yet more beautiful impediments?

— *Mike Taylor, 14 April 2010*

* * *

Mike, mcons, mcar, mcdr, set-mcar!, and set-mcdr! are provided by scheme already.  Just add (require scheme/mpair) to your definitions to get more mutable pair functionality.  You will need to use slightly different function names, e.g. mlist, mlength, etc.  The /scheme/mpair module is well documented.

When I first wrote N-Queens, returning the solutions was a requirement.  So naturally I built the solutions with cons for later return.  After my earlier post, I realized that two additional lines of code would eliminate all cons operations.  Now, it doesn't garbage collect until N = 17:

> (begin (collect-garbage) (time (N-Queens 17)))
cpu time: 6670485 real time: 6702624 gc time: 618
95815104
618ms time in gc while taking 1 hour and 51 minutes to determine 95,815,104 solutions isn't bad.

The key is:
(else
..(set-mcdr! prior-column (mcdr column))                    ..(set-mcdr! column solution)
..(begin0
....(next-queen safe-columns column new-count)
....(set-mcdr! column (mcdr prior-column))
....(set-mcdr! prior-column column)))

Gene

— *Gene Snider, 13 May 2010*

* * *

