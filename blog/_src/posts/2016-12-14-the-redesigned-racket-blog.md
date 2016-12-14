    Title: The redesigned Racket blog
    Date: 2016-12-14T13:21:20
    Tags: 

*posted by Matthew Butterick*

I love Racket. But a few months ago, I really wanted to kill this blog. 

Why? Because who reads blogs, right? It's like getting 4% interest on your savings — so 2006. This is Racket. We're from the future. No, really. We even have a [futures visualizer](http://docs.racket-lang.org/future-visualizer). Take a look. Do you visualize any blogs?

But my amigo [Sam Tobin-Hochstadt](http://homes.soic.indiana.edu/samth/) had some wise words. "MB," he said, "we're not killing the blog. But tell you what: we'll let you redesign it."

"OK! Yeah! I'll show you!"

And this is really Sam's genius. After you talk to him, not only have you entirely changed your mind, you've volunteered to do all the work.

But man, blogs. Have you seen the first blog ever, from 1994? Good news — [it's still online](http://www.links.net/vita/web/start/original.html). For the first couple years they were known as *personal websites*. Then they were called *weblogs*. Then just *blogs*. For a while after that, it seemed like everyone was starting a blog. Then everyone was redesigning a blog. Then everyone was ignoring a blog.

Racket didn't have an official blog back then — this one was started in 2007 — but there are some interesting blog-like records in the archives. For instance, the [`HISTORY.txt`](https://github.com/racket/racket/blob/ddf6985020d7dd11d17cd42b1746bd853667fc2b/racket/collects/racket/HISTORY.txt) file in the main repo. It chronicles every release back to version 0.27, in September 1995. AFAIK that code is still used today.

Today, blogging persists. But most of today's tools are calculated to make it as easy as possible. Too easy, perhaps? Not to sound curmudgeonly. But to my mind, the diaristic aspects of blogs were always incidental. Their most important purpose was to give millions of nerds a pretext to learn about web technology. In the ’70s, these nerds were typing out [BASIC computer games](http://www.vintage-basic.net/games.html). These days, I suppose they're all writing their own JavaScript front-end frameworks. Ten years from now, what — harvesting dilithium crystals in the delta quadrant? (I'm being coy. I do have a [futures visualizer](http://docs.racket-lang.org/future-visualizer).)

So, the Racket blog. My major objection is that until yesterday, it was built on the moldering skeleton of [Blogger](http://blogger.com), which has, against all odds, stumbled into this century. Merely a small cut above [Geocities](http://gizmodo.com/5983574/remember-the-hilarious-horror-of-geocities-with-this-website). Surely we Racketeers could do better.

I turned to Greg Hendershott's static-blog engine [Frog](https://github.com/greghendershott/frog). A very slick Racket package that turns Markdown source files into HTML. If you must blog — please try it.

The rest of the design draws on the typography & color themes I used for the [Racket documentation](http://docs.racket-lang.org) — still the best-looking docs in the business. For the headlines, I added something new — the [Cooper Hewitt typeface](https://www.cooperhewitt.org/open-source-at-cooper-hewitt/cooper-hewitt-the-typeface-by-chester-jenkins/), designed for the namesake Smithsonian museum. Very lovely and totally free.

Thank you to Sam Tobin-Hochstadt, Greg Hendershott, Vincent St-Amour, and Robby Findler for helping me complete this project. As Matthias Felleisen did in [his first post](/2007/05/macros-matter.html) for this blog, I dedicate my work "to all things macros and everything else that matters in Racket."

PS. I did succeed in killing the comments form. Not everything from the ’90s needs to be preserved.

<!-- more -->
