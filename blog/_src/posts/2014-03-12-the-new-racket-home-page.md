
    Title:The new Racket home page
    Date:2014-03-12T10:09:00.001-04:00
    Tags:

*posted by Asumu Takikawa*

Racket now has a new look for its web presence. You can see it at[ http://racket-lang.org](http://racket-lang.org/) (modulo DNS propagation).                                                    



The new pages improve on the old in several ways:                                                                    





* More information is now on the front page.                                                                    

* The site works much better small devices, such as phones.

* It's easier to find important parts of the site, like the package directory.

* The download process is more streamlined -- now it's only 2 clicks.



We also wanted to have a new look, and a description that talks about why Racket is exciting.



Finally, we've taken this opportunity to shift our web hosting entirely to Amazon S3, for which Greg Hendershott's [aws](https://github.com/greghendershott/aws) package has been very helpful.



While the new pages have been under development for quite a while, we're still happy to take bug reports, suggestions and (especially) patches: all of the site can be found here:

 [https://github.com/plt/racket/tree/master/pkgs/plt-services/meta/new-web](https://github.com/plt/racket/tree/master/pkgs/plt-services/meta/new-web)

with the framework for building the site here:

 [https://github.com/plt/racket/tree/master/pkgs/plt-web-pkgs](https://github.com/plt/racket/tree/master/pkgs/plt-web-pkgs)



The site is built using Eli's [scribble/html](http://www.cs.utah.edu/plt/snapshots/current/doc/scribble-pp/html.html) library, which is very nice to use.



I'd especially like to thank Eli and Matthew for their help with this -- going from my hacked-up HTML prototype to the smooth-building and well-organized code we have now has taken lots of work.



Sam