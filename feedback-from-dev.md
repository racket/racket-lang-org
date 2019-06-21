
## Arrangement/Content 

- There should probably be a description of what language oriented programming is.

- "With a Vibrant Community" could be just "Vibrant Community".

- "Strong" is kind of a strange word to describe safety and reasoning
  features. Why not "safe"? I don't like that either, but I think it's
  better.

- "little macros" and "big macros" should be adjacent.
- "general purpose" and "DSLs" should be adjacent. 

- The code snippets, e.g. under "little macros", are in too small of a font to
  read. Reminds me of talks where they just display tons of code to bamboozle
  the audience, which always annoys me.

- If I could change one thing, I would edit the first paragraph of the “Any
  Syntax” section.  Instead of “Real Racket programmers love parentheses”,
  maybe something like “Racket programmers usually love parentheses”, and
  instead of “is almost as easy as building beautiful languages”, perhaps
  “is almost as easy as building parenthesized languages”.

- ? For "Little Macros", can we have a different example? It takes me more
  than a minute to figure out what's going on in that example, even though
  I think I know macros and Racket syntax at some level. People who are new
  to Racket are going to just skip that example with confusion. 

- ? It would be cool if the typed Racket example contains a type error, and
  we show how typed Racket catches the error. 

## Mechanics 

- When trying to get the #lang examples to display again after clicking a tab
  under "the language-oriented programming language" section, my instinct was to
  click some of the whitespace off to the side to "deselect" the current tab.
  This did not work, and I had to instead click on the section heading.

- The very first thing I see is news. If I'm a regular Racket user I would
  know where to get news from. If I'm a new user, I wouldn't care about
  news. Either way, I think it should have a place in homepage but not at
  the top.

- ... trying to get all 3 ideas into view without scrolling (MF: remove news
  and get there?) 

## Code Examples 

- What's the difference between `new frame%`, `message-box`, and
  `make-object button%`.   ("Why aren't they `frame`, `message-box`, and
  `button`?"). 

- A Scribble example could be more free text, with only one or two `@(`.
  It would be nice if the Scribble example is mainly proses. Otherwise,
  users could (for example) assume that this is yet another Racket program
  will a nice string literal syntax.


- Maybe leave out the comments in the code samples ... try to read the code and guess what it does

## Looks 

- screen size problems (MF yes someone should figure this out) 

- Can we make the headers ("Racket, the Language-Oriented Programming
  Language", "Racket, the Ecosystem") change color on hovering to indicate
  that it can be clicked? 

- don't use JS for tabs (MF: I don't even know what this means)

- The big blue headings look like another header, they are more prominent than the main top page header, which looks weird

- The giant racket logo next to the code looks misplaced, the racket logo is already in the top left.

- It would be easier on the eyes if the text color was slightly darker blue

- Uses of three hyphens ("---") are not being converted to em-dashes ("—") 

## Questions 

- why isn't there a publications page? 

## Probably Useless 

- show output of programs? -- For "General Purpose", again, users want to
  see an output. A GUI window (like the "Big Macros" example) would
  suffice. 

- The JavaScript driving a change of "tabs" doesn't use the history APIs
  (e.g. history.pushState, window.onpopstate), so it doesn't work as (I)
  expected with browser-based control operators like "back" and "duplicate
  tab." Mumble about our web server and continuations 


- Regarding the second row again, I think that we should not expect users
  to click on those tabs to explore. Instead it should be presented
  statically, maybe as grid of little boxes. Web pages are much more
  pleasant when I could just scroll through it.



### Greg's Response 

Many good ideas here!  A couple suggestions:

1. I recommend not assuming screen sizes.

Right-click, choose Inspect Element, and click the little mobile icon.
Try various sizes, from a little mobile phone on up to an iPad. (This
sort of tool is available in Firefox and Chrome.)

The current experience is not good. It should be: Someone might be on a
train or plane, following a link from social media or the orange site.
Sometimes commuting or traveling is their main opportunity to learn
about new things and form first impressions.


2. I recommend not using JavaScript to implement "tabs".

This isn't a "single page web app". It is text and images. It could
simply flow through whatever size screen is available, small, medium, or
large. Let the web browser do its job. (And let the page download
faster, probably get higher search rank, etc.)


TL;DR: The current tabby thing is not helping on small screens; on large
screens it is (in my opinion) a frustrating Advent calendar. 
