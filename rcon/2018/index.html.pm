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

◊head["b"]{Kent Dybvig}

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

◊folded{
◊speaker["" "Christopher Lemmer Webber and Morgan Lemmer-Webber"]{Racket for Everyone (Else)}

Racket currently historically targets two groups: young programmers (learn by building games) and programming language theorists.  We believe Racket is well positioned to meet a wide variety of people in-between these groups.  Morgan will present on our use of Racket for non-programmers.  Using Racket, Dr. Racket, and Scribble, we have run a series of successful workshops on “programmable publishing”.  Although these were marketed to humanities students, this method can be applied to a wide variety of disciplines including the social sciences and mathematics.  It can be applied outside of the university for creative writing, community outreach, and other populations with publishing needs.  The common thread through these communities is users who don’t view themselves as programmers or who would like to program but don’t think they have the ability or resources to learn.  Because of its accessibility, we use scribble as a gateway into basic programming skills where participants can apply their fledgling skills to a practical output that relates to their daily lives.  Chris will present on Racket for the general programmer.  Racket is as batteries-included as Python and is well set up to be just as general.  How can we reach a general audience?  What can we gain by doing so?

◊bio{◊strong{Christopher Lemmer Webber} is a user freedom advocate with who focuses on network freedom.  Chris is co-editor of the W3C ActivityPub protocol which provides federated communication for decentralized social networks.  They have been programming in lisp for some time and have recently come to love Racket and have been co-running the "Programmable Publishing: Digital Humanities for Everyone" workshops using Racket and Scribble with Morgan Lemmer-Webber.}

◊bio{◊strong{Morgan Lemmer-Webber} is a PhD Candidate in the department of Art History at the University of Wisconsin, Madison. Her dissertation focuses on the real and symbolic associations between women and textile production in the Roman Empire. She has been the lead developer for the Digital Humanities project "A colonial merchant: The ledger of William Ramsay" since 2015. She has recently developed materials for and co-taught a series of workshops to teach basic programming skills to humanities majors using Racket and Scribble with Christopher Lemmer Webber.}
}

◊folded{
◊speaker["" "Eric Griffis"]{Dataflow network programming with Neuron}

Neuron is a framework for high-level network programming in Racket. Its language-oriented approach builds upon familiar abstractions like channels, ports, threads, and evaluators with support for a simple but powerful form of exploratory dataflow network programming and information flow control. In this talk, I'll show how Neuron can be used to design composable networks of communicating processes, lift procedures and modules onto the network, and integrate with existing software infrastructure like the Web.

◊bio{◊strong{Eric Griffis} is a highly intuitive, life-long programmer with an acute sensitivity to the impact of software on society. His interests include programmable software infrastructure, social software ecosystems, and software as organism versus mechanism.}
}

◊folded{
◊speaker["" "Stephen R. Foster"]{I Pivoted My Startup to Use Racket.  Can We Pivot the World?}

Every year, my company teaches coding to over 5000 students in San Diego.  Topics range from: robotics, game design, web design, virtual reality, Java, Python, physical computing, and Minecraft modding.  Why so many topics?  That's what the market wants, so that's what we provide.  In 2018, I took on the seemingly impossible challenge of unifying all our topics under a single language of instruction.  That language was Racket.  This talk reports on our use of Racket and Racket-embedded DSLs to teach a broad spectrum of computing topics. 

Due to the resounding success of our Racket pivot, I'm convinced that Racket is an educational power-tool for computer science education.  I used to say that my mission was to teach coding to every child in the world.  I can now confidently refine that mission: I want to teach Racket to every child in the world.  This talk outlines the growth vision for ThoughtSTEM -- a vision that will bring broad-spectrum Racket-based education beyond San Diego and into 20 more major American cities by the end of 2022.  Ultimately, I want to see DrRacket on every computer in America, as a basic tool that everybody knows how to use -- as familiar and useful as a web browser or a word processor.  Can we make this happen?  I think we can.

◊bio{◊strong{Stephen R. Foster}, Ph.D. is the CEO and Co-Founder of ThoughtSTEM, an NSF-funded ed-tech company whose mission is to teach computer science to every child in the world.  ThoughtSTEM has launched multiple educational titles -- LearnToMod and CodeSpells, both of which have been featured in venues such as Wired, the Guardian, and Rock, Paper, Shotgun.  ThoughtSTEM's software is used by over 100,000 people world-wide.  ThoughtSTEM also runs coding after-school programs and summer camps in San Diego, teaching over 5000 students face-to-face every year.}
}

◊folded{
◊speaker["" "David Storrs"]{Batteries Included: Commercializing Racket}

The Racket ecosystem (toolchain, libraries, documentation, etc) is surprisingly robust considering the comparatively small userbase of Racket as compared to more mainstream languages such as Java, Perl, and C++.  There are some holes in what's available, but there are some impressively esoteric items as well -- things that you might not need too often, but when you need them you *really* need them.

Biomantica Inc is a bioinformatics company built entirely on Racket.  This talk will cover our experience with Racket packages, documentation, etc, as it relates to our business.

◊bio{◊strong{David} started programming professionally in 1995 and has worked in fields as diverse as medical software, financial software, video games, publishing, and (of course) web programming.  Biomantica is his third (fourth, depending on how you count) startup.  He's been working with Racket since late 2015 and is dreading the idea of ever going back to a more mainstream language.}
}

◊folded{
◊speaker["" "Nadeem Abdul Hamid"]{Sinbad: Sailing the Waves of Data}

Sinbad is a software library, intended for use in introductory programming courses, that makes it very easy to uniformly access online data sources provided in standard formats. The library provides unified support for XML, JSON, and CSV/TSV data sources, automated caching facilities (useful for having offline access to data), sampling features (for development/testing on subsets of large data sources), and binding to instances of user-defined structures. On top of all of that, a primary pedagogical goal has been to make the interface as simple and intuitive as possible for novice programmers.

◊bio{◊strong{Nadeem Abdul Hamid} is an associate professor of computer science at Berry College.}
}

◊folded{
◊speaker["" "Pavel Panchekha"]{Verifying Web Pages with Racket and Z3}

Making a website accessible, usable, and attractive is important, but CSS's complexity makes it easy for layouts to break for some users, on some browsers, at some font sizes, or on some devices. Cassius is a new project for reasoning about all possible renderings of a web page. In this talk, I'll describe how Cassius uses the Z3 equation solver to look for mis-rendered web pages and how Racket makes it possible to implement the CSS standard accurately and concisely.

◊bio{◊strong{Pavel Panchekha} is a graduate student at the Paul G. Allen School for Computer Science and Engineering at the University of Washington, where he is a member of the Programming Languages and Software Engineering (PLSE) group. He received a BS in mathematics from MIT. His research focuses on domain-specific tools for non-expert developers, with other projects in floating-point computation.}
}

◊folded{
◊speaker["" "Jon Zeppieri"]{How to Ask for the Time in Racket}

◊bio{◊strong{Jon Zeppieri} is a software developer at CarePort Health in Boston where he has been overheard on multiple occasions asking "Yes, but 'today' in what time zone?" He discovered Racket in the late '90s, when it was called PLT Scheme, and has used it ever since.}
}


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

We will have another edition of Racketeer Office Hours, with details
to be announced.

◊h3{Sponsors (#%kernel tier)}

◊inline-list['sponsor]{
You
And
Them
}

◊h3{Sponsors (big-bang tier)}

◊inline-list['sponsor]{
◊link["https://github.com/david-vanderson/"]{David Vanderson}
◊link["https://lisp.sh"]{Jesse Alama}
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
