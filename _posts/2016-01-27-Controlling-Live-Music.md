---
layout: post
title: Controlling Live Music
tags: Music
year: 2016
month: 1
day: 27
published: true
---

If you think about, it controlling live music is far more difficult
than controlling a web browser.

If you think that reactive programming in a web browser is difficult
then think again - this is really really easy compared to control a
live-gig performance setup with two or three musicians, a dozen
synthesizers and half a dozen computers.

In the 2014 Strangeloop keynote [Our Shared Joy of
Programming](https://www.youtube.com/watch?v=3_zW63dcZB0) Carin Meier
and Sam Aaron had drones dancing to live music (look at the video 43
minutes from the start). This is **Bigtime badass reactive
programming** and is something far more difficult than anything ``reactive'' done
in a browser. This is real-world objects reacting in
real-time to messages.

One of the most difficult control problems I can imagine
involves an orchestra of 50 musicians.

An orchestral musician has two main inputs, ears and eyes, the conductor
has an advance gesture control device, a wooden stick which predates
the Internet of things and works remarkably well.
It boots instantly, requires no cloud storage, and doesn't need
firmware updates, and doesn't nag you every day about not having been backed up.

The orchestra is controlled by waving the stick.

Listening to sound from multiple instruments and transcribing it
accurately is something no computer can yet do with any accuracy.

Timing in musical performance is critical, delays of more than a few
milliseconds are noticeable, tempo shifts not so important, provided all the
musicians change tempo at the same rate.

> Music as a control problem is terribly difficult - it pushes
our understanding of interfaces to the limit.

This is why the methods and the software designed for live performance
is worthy of study - I've started looking at this in some detail
and  I think there's a lot we can learn from it, and that this
should influences how we design software and how we interact with things.

This is particularly relevant as we move into a world of the Internet
Of Thinsg.  Is there really a better way of conducting an orchestra
than waving a bit of wood, or has thousands of years of evolution and
experimentation resulted in the perfect control device - a stick?

# Music is performed by multiple instruments and musicians in real-time

So the problem is intrinsically parallel, distributed and soft real-time.

``Auugh you're thinking,'' are there any programming languages
designed to be intrinsically parallel, distributed and soft real-time?''

Answer: Yes.

So the match between Erlang and Music control is pretty obvious - it's just that
Erlang has been used for the soft real-time control of distributed telecommunication
systems and not orchestras.

I thought I'd take a look at how some systems designed for live performance look.

Several things are immediately apparent (and this is based on observation of three
systems,  [Sonic Pi](http://sonic-pi.net/),
[SuperCollider](http://www.audiosynth.com/) and
[Pure Data](https://puredata.info/) - to start with
the controls have a declarative feel - the interfaces
are simple, and these are build from many components which operate in parallel.

All the above systems interact with each other by exchanging
OSC messages over UDP, I've described this in
[A Badass Way to Connect Programs Together](http://joearms.github.io/2016/01/28/A-Badass-Way-To-Connect-Programs-Together.html)
and
[Controlling Sound With OSC Messages](http://joearms.github.io/2016/01/29/Controlling-Sound-with-OSC-Messages.html).



