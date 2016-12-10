
    Title:Marketplace: A language for network-aware programming
    Date:2013-05-29T17:43:00.000-04:00
    Tags:

*posted by Sam Tobin-Hochstadt*

We are happy to announce the release of Marketplace, a new programming language for building functional network programs.  Marketplace combines two fundamental ideas in a new way: nested virtual machines and publish/subscribe messaging. Nesting allows programs to isolate processes and to delimit conversations. While publish/subscribe generalizes point-to-point and broadcast messaging, it smoothly turns the appearance and disappearance of participants and resources into presence and absence messages. Such messages make it particularly easy to start and stop services and to manage resources based on demand.

Here is a simple TCP echo server written in Marketplace: 

```racket
#lang marketplace
 
(endpoint #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
          #:conversation (tcp-channel from to _)
          #:on-presence (spawn #:child (echoer from to)))
 
;; echoer: TcpAddress TcpAddress -> Transition
(define (echoer from to)
  (transition stateless
    (endpoint #:subscriber (tcp-channel from to ?)
              #:on-absence (quit)
              [(tcp-channel _ _ data)
               (send-message (tcp-channel to from data))])))
```

The initial endpoint subscribes to TCP messages on port 5999. When a conversational partner appears, the endpoint spawns a new process that runs an echoer process. The latter is stateless and subscribes to TCP messages. When it gets messages with payload data, it sends them back out with the opposite addressing; when the TCP conversation disappears, it quits.

Thus far, we have built several real systems using Marketplace: a DNS server, a DNS proxy, and an SSH server.

The DNS proxy has handled DNS traffic for ourselves and other members of our lab for the last several months.

You can read an overview along with detailed documentation for Marketplace at [http://tonyg.github.io/marketplace/](http://tonyg.github.io/marketplace/).

To get the sources for Marketplace as well as the applications point your browser to [https://github.com/tonyg/marketplace](https://github.com/tonyg/marketplace).

Enjoy!

Tony Garnock-Jones


Sam Tobin-Hochstadt


Matthias Felleisen

<!-- more -->



* * *

This looks really cool.  I can't wait to try it out.  How does it perform, and how robust is it against cracking attempts?

— *Geoff Knauth, 30 May 2013*

* * *

Interesting. I guess it's inspired by Erlang. What's the reason for calling this a programming language, and not just a framework or library? I look forward to more information, and tutorials.

— *Jon, 31 May 2013*

* * *

Geoff, we haven't yet tried to optimize this for performance, but the DNS proxy built with Marketplace serves all the DNS traffic for a bunch of people in our lab. As for security, that's all about what application you write using it.

— *Sam Tobin-Hochstadt, 31 May 2013*

* * *

Jon, yes, Erlang is a big inspiration, among many other things. It's a language in the sense that it goes after `#lang`, and in the sense that it extends the programming model in a fundamental way.  Ultimately, the distinction between framework, language, and library is fuzzy.

— *Sam Tobin-Hochstadt, 31 May 2013*

* * *

