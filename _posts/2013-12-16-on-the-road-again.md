---
layout: post
title: On the road again
tags: erlang conferences
year: 2013
month: 12
day: 16
published: true
summary: Review of two conferences
---

I've been on the road again. At the start of the year I said ``I'll
only talk at six conferences this year, and go to the USA once.'' 
__but it didn't work out that way ...__.

I talked at 10 conferences and went to the USA three times not once.
I've been to San Francisco, New York, Berlin, London, Vilnius and visited
established companies and start-ups that I'd never heard of.

Each visit is a mini-adventure with it's own stories. When I was in
New York I was going to speak at an Erlang factory and took the Metro
to a stop that was a four minute walk to the conference venue, then
walked off in the wrong direction, got lost and walked around for an
hour and a half before finally finding the venue.

Each new meeting is a mixture of old-faces and friends and new friends-to-be.
I have a good memory for faces, a bad memory for names, and a terrible memory
for placing faces in places.

``Hello Charles, didn't we meet in Berlin?''

``It's Mary and it was San Francisco''

How to invite me to a conference
================================

My problem with conferences is that I find it difficult to say no,
especially to my friends. My long-time friend and my ex-student
Francesco Cesarini is very persuasive. He'd tried to get me to go the
the Berlin Erlang Factory on several occasions, each time I'd said no.

Then Francesco phoned for a chat and asked again, ``Oh and by the way, you could also
talk at GOTO-2013, it's the same week, you could do two conferences with only one
trip, at Berlin is not so far away, and it's a nice city ...''

In a weak moment I agreed. I heard a slight clicking...

``Francesco, ..., Francesco''

``I was Tweeting,'' said Francesco. Two tweets followed: ``@joeerl to
talk in Berlin at GOTO and Erlang factory''
 
Which is how I ended up in Berlin.

So if you want to get me to go to a conference, nag a bit, I'll probably say no
several times, then catch me in a good mood. But be warned I'm wising up to
this strategy.

Dan North caught me in a good mood and so my next conference will be
QCon in London next March. I just have to think of something to say.

Why Going to Conferences is Fun
===============================

Firstly, you challenge yourself. I do a lot of "Conference Based
Development." You think of some catchy title that you hope will
interest people, then write the abstract so the conference organizers
can publicize the event and then forget about the whole thing.

About a month or two before the conference you have to book the trip,
and about two weeks before the conference think about the talk. This
often involves describing some program or technique that you've been
thinking about for a while, but haven't got around to implementing.

A lot of thanks get implemented this way.
[Erlang 2](https://github.com/joearms/erl2) was developed for a talk at the
Stockholm Erlang Users' conference.

I remember my co-conspirator Robert Virding asking me what a domain
specific language was, he was giving a talk about implementing DSLs in
Erlang and wondered how to go about this. He solved this by
[implementing LUA in Erlang](https://github.com/rvirding/luerl), so then
he could give a talk about this.

Implementing a programming language just so that you can give a talk
about it in a conference is pretty extreme, but you get the idea, it's
like test-driven development on steroids with an
addition adrenaline kick when you stand up in front of an audience to
test your program for the first time.

The other thing that happens at conferences is that you make new
friends and meet like-minded people. This is what I call ``brain
food.'' We need ''food for the tummy,'' and ``brain food.''
Conference food is usually awful - I hate eating while standing and
trying to talk eat and drink at the same time. I need at least six
arms to do this, but have only two.

Why going to conferences is a drag
==================================

Conferences are terribly disruptive. A one hour talk takes hours to prepare.
A decent ``off the cuff, impromptu, witty remark'' takes months of thinking about.

I'm extremely bad at multi-tasking, It would be an exaggeration to say that I can only do
one thing at a time, I can actually do about 0.8 of a thing at one time, and as for having many irons in the fire, forget it.

This is why conference talks are terribly disruptive, they totally
destroy my short-term cache, when I get back to my regular work, I need
to re-load my out swapped brain. And worse, after every conference I
meet some new people and get some new ideas that must absolutely 
get highest priority, despite whatever else I'm working on.

Highlights from CodeMesh
========================

Enough of generalities. Now I'll talk about [Code Mesh](http://codemesh.io/) held in the
beautiful Hotel Russell in Russell Square. Russell Square is in the
Bloomsbury district of London, a watering hole for writers,
intellectuals, philosophers and artists in the 1920s. Virginia Woolf
has a room named after her in the Hotel, she lived nearby, and
feminists are said to have help meetings in the hotel, so it's an
appropriate place for a conference that is largely about ideas.

The History of Lazy Functional Programming Languages
====================================

For me the highlight of the meeting was the talk given by David Turner.
I'd met David a long time ago in 1980 something, we'd invited him to Ericsson
to talk about SASL.

<img src='/images/david_turner.jpg'/>

Joe Armstrong and David Turner


David Turner is the man who ``rediscovered'' functional programming.
Perhaps rediscovered is the wrong word, but it was Davids work
that connected Churches Lambda calculus (1941) with work 
on Combinatorial logic by Curry and Freys (1958) and made the first
lazy functional programming language (SASL) in 1972.

David's SASL was inspired by Peter Landin's ISWIM. SASL was first implemented
in LISP over a weekend by Tony Davie, and reimplemeted by David in BCPL in 1973.
Lazy functional programming languages were born.

SASL was, like Erlang, dynamically typed. I asked David about this,
he seemed unconcerned. Some of his language had dynamic types, others
static dynamically inferred types, he seemed pretty neutral in the
great ``typed/untyped'' debate. ``Both have their advantages'', he
said.

SASL is the great-great-great-great-grandfather of Haskell.
The family tree looks like this
 
```
SASL -> NPL -> HOPE -> ML -> KRC -> Miranda -> Haskell 
```

David made SASL and KRC (Kent Recursive Calculator) and Miranda.

David is now retired, but is still bright as a button. I met him in
The Erlang solutions offices where he was doing a Raspberry Pi course.
During the conference we had many conversations, remembering old times and
colleagues and speculating abut the future.

You can read more about the history 
[here](http://www.cs.kent.ac.uk/people/staff/dat/tfp12/tfp12.pdf).

50 Shades of Green
==================

Dave Thomas and Jose Valim gave a highly entertaining talk ``50 shades of green''
This was about the limits of language, and how words and the meaning of words 
limit what we can think about. 

They showed some video footage from a BBC program that filmed the
Himba of northern Namibia - they call the sky black and water white,
and blue and green share the same word.

The Himba have distinct names for different shades of green, we have no names 
for these shades, so we don't ``see'' the different colors, but the Himba do.

Dave and Jose related this to concepts on programming languages, how
different words have acquire precise meanings which are misunderstood 
in different communities.

How true this is. For me the words ``concurrent,'' ``parallel'' and
``simultaneous'' have completely different meanings, but many people
think they mean the same thing.  It's like me seeing three shades of
green, when the person I'm talking to sees one green.

Communication is difficult, especially when you can't get inside other
people's heads, and see the world through their eyes.

Are You Ready for 1000-Way Parallelism on a Single Chip?
=========================

Andreas Olofsson's talk asks if we were ready for the 1000 core
chip. Well the answer is ``Yes what took you so long.''

Ever since I saw the Intel Polaris Chip (2007) I've been totally
convinced that network on chip (NOC) architectures were the way to go.

A NOC architecture is a 2-D regular matrix of processors each with a
local cache, and with a high speed chip-to-chip interconnection bus.

The beauty of the chip is it's regularity. Instead of a single very
fast chip with loads of memory and a complex instruction set, we use
the chip area to make a large number of small slow processes.  In
doing so we can dramatically decrease the power needed to run the
chip and we can put a large number of processors on the same chip.

The Intel Polaris achieve one tera-flop using 62 Watts in 2007.  See
the article on [Tera-Scal
computing](http://software.intel.com/en-us/articles/tera-scale-computing-a-parallel-path-to-the-future).
This was amazing. But what happened since then? Pretty much nothing
on the Intel front. This is hardly surprising. Intel has no room in
it's portfolio for a chip that makes all their other chips worthless.

The problem with the NOC chips is that the software is not ``business
as usual'' the NOC has a non-shared memory architecture and virtually
all software today is based on the idea of a very large (and possibly
shared) memory space.  NOCs have a fundamentally different
architecture, they are non-shared-memory message passing
architectures. These are the chips that Erlang has waited for.

When a history of the last 5,000 years of computing is written in the
future, shared memory will be viewed as an aberration. The first
computers had non-shared memory, then came shared-memory, then back
again to non-shared memory and pure message passing architectures.
NOC architectures and agent languages will lead us out of
shared-memory hell.

Next comes [Tilera](http://www.tilera.com) - with the core 64 chip. We
got hold of Tilera chips as soon as we could lay our hands on them and
ran Erlang on the chips. We've had some pretty good results with
Tilera. One program we were interested in ran 33 times faster on a 64
tile machine, without any tweaking the code.

The next in line is the [Epiphany
chip](http://www.adapteva.com/products/silicon-devices/e16g301) that
Andreas talked about. The [Parallela
board](http://www.parallella.org/) is the Raspberry pi of parallel
computing, and it ships with Erlang. Erlang is not used in the NOC CPUs
but in the control system that sits beside the NOC chip.

<img src='/images/parallela.jpg'/>

The Parallel board caused a lot of excitement at the
conference. Here's a picture of a few guys in headed discussion round
the board. There are nine parallela boards at the front of the
table. Here I am in discussion with Krestin Krab Thorup and Andreas
Olofsson. We were talking about Huffman encoded instruction sets,
it's a shame language designers don't talk more often to chip designers.

It seems like chip designers and language designers live in different
parallel universes. Chip designers gleefully add weird instructions to
their CPUs that nobody has a clue what to do with, and the leave out
the instructions we really want. 

Andraes has made the chip, but hasn't talked to any language
designers. Amazing. Even David Turner got interested and wants a
parallela board. He wants to implement a parallel graph reduction
machine on the chip.

Andraes roadmap was amazing. Today the Epiphany-III (16 core) and
Epiphany-IV (64 core) chips are ready, next in the pipeline is a 1024
core machine with a 1MB cache memory per core.

I've waited 25 odd years for this machine, so I was pretty excited to
meet Andreas and learn about the Parallel board and the Epiphany
chip. I hope Santa brings me one.

Next stop Vilnius
================= 

With Code Mesh finished, it was ``next stop Vilnius''. Many of the
conversations that started in London, got continued in Vilnius. I
found to my surprise that many of the speakers at Code Mesh had
continued to Vilnius to talk at [BuildStuff](http://buildstuff.lt/).

It's a strange feeling to start a conversation in London, be
interrupted and continued in Vilnius. The people remain the same, but
the places and backgrounds change. Some faces remain, others drop out,
new faces emerge.

So it was bye bye to David Turner, Dave Thomas, Jose Valim, Bruce Tate
and Franceso Cesarini, carry on talking to Bodil Stokke, Torben
Hoffmann, Jonas Boner, Sam Aaron and Pieter Hintjens and ``Hello,
haven't seen you for a while,'' to John Hughes.

Each new conference brings with it new people, new ideas and new
experiences.

In London I started a new conversation with Pieter Hintjens, who very
kindly gave me a copy of his book [Culture and
Empire](https://www.createspace.com/4484521). Pieter is an unusual
combination of thinker, programmer and writer. I'll write a review of
his book when I've read it - thanks Pieter.

Conferences are brain food - so thanks to everybody I met and talked to
in London and Vilnius, I guess I'll be carrying on the conversations we have
some time next year.




