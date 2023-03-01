#lang pollen

◊(require (only-in "index.html.pm" top)
          racket/string)

◊(define (email user . host)
   (define addr (format "~a@~a" user (string-join host ".")))
   ◊link[(format "mailto:~a" addr) addr])

◊(top)

◊section[#:class "one-column-body-text"]{
Friendly Environment Policy

The Racket community aims to improve the world through programming. It started with the goal of introducing everyone to the wonderful world of program design, with a spirit of full inclusion and no emphasis on any specific group.  Over time it has grown into a full-fledged professional community with a well-known reputation for helpfulness and openness on its online communication channels.

Racket project management wants to encourage and preserve this open exchange of ideas, which requires an environment that enables all to participate without fear of personal harassment. We define ◊em{harassment} to include specific unacceptable factors and behaviors listed in ◊link["https://www.acm.org/about-acm/policy-against-harassment"]{the ACM's policy against harassment}. Unacceptable behavior will not be tolerated.

Anyone witnessing or subject to unacceptable behaviour should notify the moderation team or Racket Project management.

◊div{Community moderation team:}
◊div[#:style "padding:0.5rem"]{
◊link["https://github.com/countvajhula"]{Siddhartha Kasivajhula} (◊email["sid" "countvajhula" "com"])
 ◊link["https://github.com/jackfirth"]{Jack Firth} (◊email["jackhfirth" "gmail" "com"])
 ◊link["https://github.com/samdphillips"]{Sam Phillips} (◊email["samdphillips" "gmail" "com"])
 ◊link["https://github.com/spdegabrielle"]{Stephen De Gabrielle} (◊email["spdegabrielle" "gmail" "com"])
 ◊link["https://github.com/jryans"]{J. Ryan Stinnett} (◊email["jryans" "gmail" "com"])
}

◊link["team.html#management"]{Racket project management} can be reached at ◊link["mailto:management@racket-lang.org"]{management@racket-lang.org}.

◊p[#:id "bottom"]{The wording of this policy is directly derived from ◊link["http://snapl.org/2015/policy.html"]{that of the SNAPL conference}, which is in turn derived from that of the ACM, with thanks to both.}
}
