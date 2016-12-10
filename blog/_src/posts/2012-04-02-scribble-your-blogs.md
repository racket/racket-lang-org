
    Title:Scribble Your Blogs
    Date:2012-04-02T18:46:00.000-04:00
    Tags:

*posted by Ryan Culpepper*

Scribble is a great language for writing documentation. Now it's a great language for writing blog posts, too. I've just released a tool called Scriblogify that compiles Scribble documents and posts them as blog entries. Scriblogify is a more polished and automated version of the scripts I've been using for several months to prepare posts for [my own blog](http://macrologist.blogspot.com).

To get Scriblogify, just download it from [PLaneT](http://planet.racket-lang.org/):

`(require(planetryanc/scriblogify:1))`

or 

`raco planet install ryanc scriblogify.plt 1 0`

The package automatically installs a raco subcommand (`raco scriblogify`) that can be used to configure Scriblogify and process and upload blog posts.

Configure Scriblogify by running

`raco scriblogify --setup`

That will open a browser window with the Scriblogify configuration servlet. The servlet will prompt you to authorize Scriblogify to access your Blogger and Picasa Web Albums accounts (only the Blogger/Picasa combination is currently supported) and then create one or more profiles—named combinations of blogs and web albums to upload to.

Scriblogify automatically handles images computed in your Scribble documents by uploading them to a web album. For example, here are some images computed with the slideshow/pict library:

```racket
> (require slideshow/pict)
> (define rainbow-colors
    '("red" "orange" "yellow" "green" "blue" "purple"))
> (for/list ([c rainbow-colors])
    (colorize (filled-rounded-rectangle 20 20) c))
'(image image image image image image)

> (for/list ([c rainbow-colors]
             [dir (in-cycle '(right left))])
    (standard-fish 25 25 #:color c #:direction dir))
'(image image image image image image)

> (cc-superimpose
   (cc-superimpose (cloud 100 80 "lightblue")
                   (cloud 90 70 "white"))
   (hc-append 10
    (standard-fish 30 30 #:color "red" #:direction 'right)
    (standard-fish 25 20 #:color "blue" #:direction 'left)))
image
```


By Scribbling your blog entries, you get Scribble's nice code formatting, colorizing, and documentation links for free—well, once you've updated your blog's CSS (see below). If you're blogging about bleeding-edge work, there's an option to make Scriblogify link to the [nightly build docs](http://pre.racket-lang.org/docs/html/) (updated daily) instead of the [release docs](http://docs.racket-lang.org/) (updated every 3 months).

Scriblogify's documentation has more details, including how to update your blog's CSS for Scribbled content and what bloggable Scribble documents look like.You can see the source for this blog post [here](https://github.com/rmculpepper/scriblogify/blob/v1.0/samples/scribble-your-blogs.scrbl). This blog entry was created with the following command:raco 
`scriblogify -p the-racket-blog scribble-your-blogs.scrbl`

Now go forth and Scribble your blogs.

<!-- more -->



* * *

This comment has been removed by the author.

— *Unknown, 2 April 2012*

* * *

This is awesome! I actually rolled my own Scribble-blog hack at http://cscheid.net (click on any post and you can see the scribble source). 

If I could run this on my own backend, I'd love to get rid of my kludgy setup.

— *Unknown, 2 April 2012*

* * *

