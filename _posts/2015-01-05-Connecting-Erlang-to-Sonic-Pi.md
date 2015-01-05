---
layout: post
title: Connecting Erlang to the Sonic Pi
tags: erlang sonic pi
year: 2015
month: 01
day: 5
published: true
summary: How I connected Erlang to the sonic Pi
---

Last September after giving [my keynote at
StrangeLoop](https://www.youtube.com/watch?v=lKXe3HUG2l4) I bumped
into my old friend [Sam Aaron](https://twitter.com/@samaaron). As
usual he was bubbling over with enthusiasm, he'd been making a music
program for kids [Sonic Pi](http://sonic-pi.net/) which interested me.

I'm interested in all things musical and especially making noises with computers
so I was _very_ interested in Sonic Pi.

My first thought was ``how can I control Sonic Pi from Erlang?'' -- with
Sam at hand this turned out to be really really easy.

``All you have to do,'' said Sam ``is open UDP port 4557 and send it
an OSC message.''

``What message?'' I asked.

`"\run code $string"` said Sam. Where `$string` is whatever you would
have typed into the Sonic Pi control window.

``That's easy,'' I thought and it was. All I needed was a library to
convert Erlang terms to OSC messages. After a little searching I found
a library at [erlang-osc](https://github.com/mujaheed/erlang-osc).

The rest was easy.

I'd complete forgotten about this until earlier today when
[@WadeMeadling](https://twitter.com/@WadeMealing)
tweeted me asking if the code was documented anywere. Well it wasn't and now it is.

I've put the code up at [sonic_pi_inteface](https://github.com/joearms/sonic_pi_interface)
it's been tested with sonic Pi version 2.0 and seems to work.

# On the Sonic Pi Interface

I'd now like to take a moment and heap praises upon the design of
Sonic Pi. It literally look me about ten minutes to interface Erlang to Sonic Pi.

_Actually this is not quite true_ It took me ten minutes after Sam had explained how the
interface worked - he had not documented the interface, assuming that nobody would
wish to remotely control the program.

The interface is simplicity itself. Open a UDP port (in this case port
4557) and send it a fragment of Ruby (as an OSC message). Would that
all programs were that easy to interface.

I have had the misfortune to interface Erlang to the apple core-audio and core-midi
interfaces - now this (interfacing to core-audio) is not for the faint hearted.

Applications using core audio must be liked in memory to the core audio framework,
a practice which is bizarre and which makes interfacing to languages that do not
have the memory layout conventions of objective-C a pain.

This (the interfacing) harks back to the old arguments of ``shared
memory'' verses ``message passing'' concurrency. Linking things in
memory and using callbacks is painfully difficult and highly error
prone.

This practise (shared memory) comes from the days when efficiency _was_ a problem,
when we had computers with MHz clocks and tens of KBytes of memory. Those days
are long gone.

Using a message passing interfacing to Sonic Pi was pure joy. Open a
port, send it a message, done.  Easy.

If I were defining an audio API Id be able to say:

    piano ! {play_note, C}

From my favorite programming language (Erlang) and it would just work.





