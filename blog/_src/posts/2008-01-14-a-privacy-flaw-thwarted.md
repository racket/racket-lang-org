
    Title:A Privacy Flaw, Thwarted
    Date:2008-01-14T15:21:00.001-05:00
    Tags:

*posted by Shriram Krishnamurthi*


My student Brendan Hickey recently identified the following security
hole.



A university (let's call it Orange University) wants to let its
graduating students vote on their graduation speaker.  They used to do
it by paper; catching up with the times, they now do it on the Web.



They used to have a box into which you could type the name of your
nominee.  But that is surely problematic: people misspell names, you
have to argue about how to count ambiguous votes, someone will vote
for their pet bonobo, etc.  Better (perhaps) to give them the names of
all the students and let them choose.  [Alert: if Orange U adopts a
simple naming scheme for email addresses, a student can immediately
screen-scrape a pretty plum list to sell a spammer.  Brendan and I
noticed this in a femtosecond; I don't know why this didn't occur to
the university.]



Anyway, now you have a Web page where people are going to choose, and
the software that processes the responses must distinguish between
these choices.  You have to associate a key with each student.  You've
already got a key for each candidate: their student ID number.  So you
use that as your key.  Now anyone viewing the page source can
immediately see which student ID number goes with which student name.
So much for confidentiality.



Whoops.



I'd like to point out that Pete Hopkins's
`send/suspend/dispatch`, and the improved version of it by
Jay McCarthy, identify and solve just this code structuring problem in
a way that the privacy leak can never occur.  For the most up-to-date
presentation of it, read section 3.2 and section 4 of
[our
paper](http://www.cs.brown.edu/~sk/Publications/Papers/Published/khmgpf-impl-use-plt-web-server-journal/).



Maybe Orange U should be using Scheme.


<!-- more -->



* * *

Salting and hashing the student ids before publishing them would protect identity as well, without depending on program-flow strategies, no?

— *fawcett, 22 January 2008*

* * *

Salting and hashing ids was a thought I had too.  But I think the paper's Continue web server has control flow constructs and error recovery capabilities built-in that would have to be added to other web-based approaches.

— *Geoff Knauth, 22 February 2008*

* * *

