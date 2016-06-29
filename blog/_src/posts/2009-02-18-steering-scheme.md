
    Title:Steering Scheme
    Date:2009-02-18T10:49:00.003-05:00
    Tags:

*posted by matthias*

Election time is here again. A couple more days and the Scheme community
will have a set of new steer-ers.

What should we want from a steering committee? 

I have argued at this place before that good language design needs a
feedback loop. Language designers write down specs; language implementers
translate those specs into compilers and interpreters; programmers use
these implementations to produce useful software. The loop comes in when
implementers inform designers of flaws, inconsistencies, mistakes, errors,
and other internal consistency problems in the specs. This is clearly
happening with R6RS, and it is good. Implementers are a biased bunch,
however. After all, they work on just one kind of program, and in a highly
specialized domain that has been mined for a long time. How can you trust
them? [*]

The loop becomes truly useful when people write large software systems (not
just compilers, because they really are special cases!)  and find that the
language fails them in some way. Such failures can come in a number of
flavors. For a document such as R6RS, we should hope that programmers can
discover problems with porting systems that are apparently due to
ambiguities, flaws, mistakes, roaches in the actual document (as opposed to
a specific implementation). 

So what does this have to do with the steering committee? 

The last thing we want from a steering committee is a radical commitment to
change (whatever it may be); a prejudice concerning R6RS; a closed
mind about the size of "Scheme" (it's too large, it's too small); a
willingness to steer without making observations. A steering committee
of overbearing curmudgeons is _not_ what we want. 

What we do want is a committee that is willing to figure out how the
listening is going to happen; how we can possibly finance a systematic way
of listening (writing NSF grants, anyone?); how the feedback is best
channeled into language design. 

Let's hope we get such a  steering committee. The Scheme community deserves it. 

 

[*] No I am not naive enough to think that language implementers don't
design languages, and that implementers don't program systems other than
implementations. I am just skeptical that it is easy for them to separate
out their various roles in an objective manner, even if many of them are
able to think at several different levels about programs.