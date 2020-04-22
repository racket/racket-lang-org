#lang scribble/base
@(require scribble/core
          scribble/html-properties
          net/url-string
          racket/list)

@(define (section* #:tag [tag #f] . c) (apply section #:style 'unnumbered #:tag tag c))
@(define (htdp) @hyperlink["https://htdp.org/"]{How to Design Programs})

@(define (during-school . l)
   null
   #;@elem[#:style "duringSchool" l])

@(define dorm-url "https://docs.google.com/forms/d/e/1FAIpQLSe6B0oqHHN3ErB1kBmQQd-CKWvHOO3E1F5yxc-xCThYggNbPg/viewform?usp=sf_link")
@(define finaid-url "https://docs.google.com/forms/d/e/1FAIpQLSeXBJ-CdvHjjVpdWP4WWjHiMUsPvvoRuexNqIY1JrrVLXSpkQ/viewform?usp=sf_link")

@(define page-style
   (make-style
    #f
    (list
     'non-toc
     'no-sidebar
      ;; Redundant if 'no-sidebar is included, but makes a reasonable TOC otherwise:
      'no-toc 'toc-hidden
      (make-css-addition (string->url "https://fonts.googleapis.com/css?family=Martel|Open+Sans"))
      (make-css-addition #"body { background: #EEc;
                                  background-image: url(https://racket-lang.org/img/racket-logo.svg);
                                  background-repeat: no-repeat; 
                                  background-size: 10em 10em; 
                                  background-position: 1em 1em; 
                                  font-size: 12pt; }
                           .navsettop, .navsetbottom { display: none; }
                           .maincolumn { background: white; padding: 2ex; 
                                         margin-top: -2ex; width: 45em; margin-left: 12em; }
                           .main, .refcontent, .tocview, .tocsub, .sroman, i { font-family: 'Martel', serif; }
                           .refcolumn { background: none; border: 0; }
                           .duringSchool { color: #B00; }
                           .price { color: #800; }
                           .cancel { color: red; }
                           a { text-decoration: none; }
                           h1, h2, h3 { font-family: 'Open Sans', sans-serif; }
                           h3 { border-top: 1px solid #888; }"))))

@title[#:style page-style]{@larger{@elem[#:style "cancel"]{CANCELED for 2020}}
                           @linebreak[] @larger{The Racket School of Semantics and Languages}
                           @linebreak[] @elem[#:style "cancel"]{not} July 27-31, 2020 
                                        @hspace[4] @elem[#:style "cancel"]{not} Salt Lake City, Utah, USA}

@;{
@margin-note{@(tabular
               (list (list @italic{@nonbreaking{Thanks to our generous supporters:}})
                     (list @larger{SIGPLAN})))}
}

@during-school{@bold{New}: Local details for participants highlighted below.}

@section*{Overview and Audience}

The
@hyperlink["https://racket-lang.org/"]{Racket}
team has spent over thirty years developing and refining a
coherent intellectual framework of programming language and programming research. For
the past few years, the team has run a summer school for developers,
academics, and undergraduates on various aspects of this framework.

@bold{This year's topic:} The 2020 school will introduce participants to
the Racket method of ``hardening'' software by going from untyped
scripts to fully typed programs.  The team has actively worked on this
framework for the past 20 years, making Racket the first language to include a
system of higher-order behavioral software contracts (2002) and gradual
types (2006). Both systems have evolved over the past two decades, and the
Racket community has gathered significant practical experience with them.

Concretely, the 2020 Racket Summer School will cover three primary topics: 
@itemlist[
@item{mostly-functional scripting and programming with modules;}
@item{behavioral software contracts; and}
@item{gradual (``migratory'') typing.}
]
For each topic, participants will gather significant practical, hands-on
experience and will get some exposure to the underlying theory. 

The ideal attendee has Racket experience beyond @htdp[] and
@hyperlink["https://realmofracket.com"]{Realm of Racket}. He or she is
@itemlist[

@item{a professional or hobbyist programmer with an interest in
strengthening his or her Racket skills;}

@item{or a graduate or undergraduate student who is intrigued by the idea
of how academic research can produce a widely used artifact and how such an
artifact can pose amazing research problems;}

@item{or an instructor who wishes to use more than the @htdp[] languages in
the curriculum, say for Programming Languages, Software Development, or
AI;}

@item{or even a researcher (post-doc, faculty) who wishes to understand the
Racket method of hardening software.}  
]

@section*{Dates and Location}

The School will run July 27-31 (Monday-Friday) at the
@hyperlink["https://www.cs.utah.edu/"]{University of Utah}.

The University is located in lovely
@hyperlink["https://www.visitsaltlake.com/"]{Salt Lake City}, Utah, USA.

Utah is home to several US National Parks, Monuments, and Sites, the Sundance Film Festival, the Bonneville Salt Flats, and much more. Learn more
@hyperlink["https://travel.utah.gov/"]{here},
@hyperlink["https://utah.com/"]{here},
or
@hyperlink["https://www.visitutah.com/"]{here}.

@during-school{The summer school will be held in room 2230 of the
@hyperlink["https://goo.gl/maps/bpWGz93472s"]{Warnock Engineering Building (WEB)}.}

@during-school{To get to campus, one option is
@hyperlink["https://www.rideuta.com/Services/TRAX"]{TRAX light rail}
to either the @hyperlink["https://goo.gl/maps/UnTF4ZSykBn"]{Fort
Douglas} or Stadium stop. Walk to WEB from there, or a
@hyperlink["https://www.uofubus.com/"]{free campus shuttle} stops at
TRAX stations and near WEB.}

@section*{Faculty}

The School will be taught by
@hyperlink["https://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen},
@hyperlink["https://www.eecs.northwestern.edu/~robby/"]{Robby Findler},
@hyperlink["https://www.cs.utah.edu/~mflatt/"]{Matthew Flatt},
@hyperlink["https://jeapostrophe.github.io/home/"]{Jay McCarthy},
@hyperlink["https://samth.github.io/"]{Sam Tobin-Hochstadt}, 
and guest lecturers (TBD). 

@section*{Week Schedule}

Each teaching day will run roughly @bold{9am to 5pm}, divided into
four sections of about 75 min each. Some sections will be organized as
traditional @bold{lectures}. In @bold{lab sessions}, the summer school
participants will practice the lecture material with hands-on
exercises; teaching assistants will be on hand to assist. Depending on
demand, the lecturers will also organize ad hoc break-out sessions on
topics of interest to the participants. 

@;{

@(define cont @italic{cont'd})
@(define (add-seps l) (for/list ([r (in-list l)])
                        (let loop ([r r])
                          (cond
                           [(null? (cdr r)) r]
                           [(eq? (cadr r) 'cont)
                            (list* (car r) 'cont (loop (cdr r)))]
                           [else
                            (list* (car r) @hspace[4] (loop (cdr r)))]))))
                              

@tabular[#:style 'boxed #:row-properties '(bottom-border ()) #:column-properties '((right-border right) ())
@(add-seps
  (list

    @list[""                @bold{AM 1}                   @bold{AM 2}       @bold{PM 1}               @bold{PM 2}]

    @list[@bold{Monday@'nbsp}     "Operational semantics"   "Redex"               "Operational semantics"   "Redex"]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Tuesday@'nbsp}    "Mystery 1: discovering"  "Mystery 1: modeling" "Mystery 2: discovering"  "Mystery 2: modeling"]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Wednesday@'nbsp}  "Mystery 3: discovering"  "Mystery 3: modeling" "Semantics reengineering" @italic{break}]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Thursday@'nbsp}   @tt{define-syntax}        (para @tt{#lang} ", " @tt{#%app})     "building your own language and IDE" 'cont]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Friday@'nbsp}     (para @tt{#lang} " from Redex")  "Generate tests"      "Test your language" 'cont]

))
]

}

@during-school{Breakfast (bagels and fruit) will be available by 8:30
each day, and lunch will be delivered.}

@section*{Accommodation}

We have arranged for subsidized lodging in dorms at the University of
Utah. To request dorm accommodation, please @hyperlink[dorm-url]{fill
out the form}.


@; @section*{Parking}

@during-school{Participants can park in the Merrill Engineering
Building (MEB) lot to the north of WEB and MEB. Contact the organizers
for a virtual parking pass.}

@section*{Tickets}

@hyperlink["https://www.eventbrite.com/e/racket-school-2020-tickets-97397698137"]{Buy ticket at Eventbrite}

@(define (price s) @elem[#:style "price" s])

@nested[#:style 'inset
@tabular[
 #:sep @hspace[2]
 #:column-properties '(top top top)
 (list
  (list @price{$499} @elem{@bold{base}} @elem{Available to all.})
  (list "" "" "")
  (list @price{$699} @elem{@bold{patron}} @elem{Covers our full cost of offering these events,
                                plus a little extra to help support Racket development;
                                Racket is part of the
                                @hyperlink["https://sfconservancy.org/"]{Software Freedom Conservancy}.})
  (list "" "" "")
  (list @price{$249} @elem{@bold{academic}} @elem{Available to participants from academic institutions that need a subsidy;
                                  tuition and travel reimbursement available for qualifying students
                                  who @hyperlink[finaid-url]{apply for financial aid}.}))
]]

Eventbrite fees are added during checkout.

@section*{Previous Racket Schools}

@hyperlink["https://school.racket-lang.org/2019/"]{2019}
@hspace[3]
@hyperlink["https://summer-school.racket-lang.org/2018/"]{2018}
@hspace[3]
@hyperlink["https://summer-school.racket-lang.org/2017/"]{2017}
@hspace[3]
@hyperlink["https://redex.racket-lang.org/summer-school.html"]{2015}

