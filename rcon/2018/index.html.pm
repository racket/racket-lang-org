#lang pollen
◊(require racket/file pollen/cache)

◊head["a"]{Eighth}
◊head["b"]{RacketCon}
◊head["c"]{St. Louis 2018}

◊gap[1]

◊(define this-rc-date "29–30 September 2018")

◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊◊xlink{Register}}
◊h2{◊xlink{Schedule}}

◊code{(eighth RacketCon)} is the meeting for everyone interested in ◊link["http://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["http://felleisen.org/matthias/manifesto/"]{world’s first ecosystem} for developing and deploying new languages.

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.

◊h3{Live Stream}

We plan on live streaming the event, as we have in the past.

◊h3{◊xtarget["speakers"]{Keynote}}

◊head["b"]{To be}
◊head["b"]{announced}

◊h3{◊xtarget["venue"]{Venue}}

This year, RacketCon will join the ◊link["https://icfp18.sigplan.org"]{International Conference on Functional Programming} (ICFP) and ◊link["https://www.thestrangeloop.com/"]{Strange Loop} for a week of programming revelry in St. Louis, Missouri at the ◊link["http://curiocollection3.hilton.com/en/hotels/missouri/st-louis-union-station-hotel-curio-collection-by-hilton-STLCUQQ/index.html"]{Union Station Hotel}. Specifically, we are in the Jeffersonian and Knickerbocker rooms.

◊h3{◊xtarget["register"]{Register}}

◊schedule[
◊row{◊at{◊span[#:style "font-weight: bolder"]{Early bird}} ◊desc{◊strong{$75 individual · $45 student (until September 8)}}}
◊row{◊at{Standard} ◊desc{$100 individual · $60 student (after September 8)}}
]

Buy your ticket at ◊link["https://tinyurl.com/RacketCon2018Tickets"]{TBA}.

RacketCon attendees also get a fantastic ◊link["http://group.curiocollection.com/RacketCon2018"]{group rate} at the ◊link["http://curiocollection3.hilton.com/en/hotels/missouri/st-louis-union-station-hotel-curio-collection-by-hilton-STLCUQQ/index.html"]{Union Station Hotel} itself.

◊h3{◊xtarget["speakers"]{Speakers}}

◊div{
◊folded[#:open #t]{
◊speaker["" "Your Name Here"]{Your Next Amazing RacketCon Talk}

This past year, you did something amazing with Racket. Now it is the time to share it.

◊bio{◊link["https://qph.fs.quoracdn.net/main-qimg-3abb243ec3f0aad890ce845a02fa99fb"]{You} are an amazing and precious programmer with a great deal to offer the RacketCon community. Please contact the ◊link["mailto:jay.mccarthy@gmail.com"]{organizers} with the full details of your talk.}}

}

◊h3{◊xtarget["schedule"]{Saturday September 29}}

◊schedule[
◊row{◊at{8:30–9:30} ◊desc{Breakfast & Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: TBA}}
◊row{◊at{10:30–11:00} ◊desc{Break}}
◊row{◊at{11:00–12:00} ◊desc{
Amazing Talk 1
Amazing Talk 2
Amazing Talk 3}}
◊row{◊at{12:00–14:00} ◊desc{Lunch}}
◊; ◊row{◊at{14:00} ◊desc{Contributor Awards}} ◊; if we have them
◊row{◊at{14:00–15:00} ◊desc{
Extraordinary Talk 1
Extraordinary Talk 2
Extraordinary Talk 3}}
◊row{◊at{15:00–15:15} ◊desc{Break}}
◊row{◊at{15:15–16:30} ◊desc{
Superb Talk 1
Superb Talk 2
Superb Talk 3
Superb Talk 4}}
◊row{◊at{16:30–16:45} ◊desc{Break}}
◊row{◊at{16:45–18:00} ◊desc{
Wow, That's a Racket 1
Wow, That's a Racket 2
Wow, That's a Racket 3
Wow, That's a Racket 4
}}
]

◊h3{Saturday evening}

◊schedule[
◊row{◊at{19:00–23:00} ◊desc{Food & drink at local destination.}}
]

◊h3{Sunday September 30}

We anticipate another edition of Racketeer Office Hours, but details
are to be announced.

◊h3{Sponsors (#%kernel tier)}

◊inline-list['sponsor]{
You
And
Them
}

◊h3{Sponsors (big-bang tier)}

◊inline-list['sponsor]{
Me
And
Us
}

◊h3{Friendly Environment Policy}

The Racket community aims to improve the world through programming. It started with the goal of introducing everyone to the wonderful world of program design, with a spirit of full inclusion and no emphasis on any specific group.  Over time it has grown into a full-fledged professional community with a well-known reputation for helpfulness and openness on its on-line communication channels. The organizers want to encourage an equally open exchange of ideas at RacketCon, the community's in-person meet-up.

This spirit requires an environment that enables all to participate without fear of personal harassment. We define harassment as unwelcome or hostile behavior, that is, behavior that focuses on people instead of ideas. ◊link["http://www.acm.org/sigs/volunteer_resources/officers_manual/anti-harassment-policy"]{The ACM's anti-harassment policy} lists a variety of specific unacceptable factors and behaviors. The organizers consider responses such as “just joking,” or “teasing,” or being “playful” as unacceptable.

Anyone witnessing or subject to unacceptable behavior should notify one of the RacketCon organizers (Matthew Flatt or Jay McCarthy.)

If a participant engages in harassing behavior, the conference organizers may take any action they deem appropriate, from a warning of the offender to an expulsion from the conference [without refund].

[The wording of this policy is directly derived from ◊link["http://snapl.org/2015/policy.html"]{that of the SNAPL conference}, with thanks.]

◊h3{Sponsoring}

If you would like to sponsor, please contact the ◊link["mailto:jay.mccarthy@gmail.com"]{organizers}. The ◊code{#%kernel} tier's names and logos will appear on all RacketCon printed materials and advertisements.

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2017}
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}

◊gap[1]

◊em{"I was wandering through the darkness, groping for anything to hold on to. The heat and exhaustion was unbearable and I felt that I could not move any longer. At the last moment before collapse, my fingers just barely touched something cold and solid. I stumbled forward and grasped at it with all my being. It was a door knob. When I opened the door, I stumbled through and felt a cool comfort that was indescribable in the moment. When my eyes acclimatized to my new surroundings, I realized that I was at RacketCon."}

— Satisfied Customer, RacketCon 2017
