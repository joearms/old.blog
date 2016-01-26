---
layout: post
title: Controlling Sound with OSC Messages
tags: OSC Networking
year: 2016
month: 1
day: 29
published: true
---

In earlier articles I have talked about 
[Controlling Live Music](http://joearms.github.io/2013/03/27/Controlling-Live-Music.html)
and
[A Badass Way to Connect Programs Together](http://joearms.github.io/2016/01/28/A-Badass-Way-To-Connect-Programs-Together.html)


In this article I'll show how I have interfaced Erlang to the
[Sonic Pi](http://sonic-pi.net/), the
[SuperCollider](http://www.audiosynth.com/) and
[Pure Data](https://puredata.info/).

The code for described here is available at
https://github.com/joearms/music_experiments

The code I'm described is at ``proof of concept'' stage - you can
download it and run it - but there are no build instructions, so
familiarity with Erlang/OSC and the target systems is implied.

# Act1 : Connecting Sonic PI to Erlang.

This all started when Sam Arron showed me how to do remotely control
Sonic Pi.

> ``It's easy,'' he said ``<span style='color:red'>Just open port 4557 and send it
a run_code message</span>.'' Actually he didn't say *exactly* this
but he might say this if I asked again.

So it's really really easy. Any of the programs you see in a sonic pi window
can be turned into a fragment of Erlang code

Here's a very simple program in Sonic Pi:

<img src="/images/sonic_pi.png">

To run this in Sonic Pi you just press the run button, and you'll hear
a noise.

Here's the code to do the same thing in Erlang:

````
test1() ->
    run_code("use_synth :fm\nplay 50\n").

run_code(Prog) ->
    %% Prog is a io-list
    P1 = lists:flatten(Prog),
    M = ["/run-code" , "erl-id", P1],
    E = osc:encode(M),
    {ok, Socket} = gen_udp:open(0,[binary]),
    ok = gen_udp:send(Socket, "localhost", 4557, E),
    gen_udp:close(Socket).
````

The entire program is in
[sonic.erl](https://github.com/joearms/music_experiments/blob/master/sonic.erl)
the OSC encoder
is in 
[osc.erl](https://github.com/joearms/music_experiments/blob/master/osc.erl).

**Fantastic** I say.

> Note: I can collaborate with the Sonic PI without messing in the
Sonic Pi source code tree - all I do is send it messages that it understands.

# Act2 : Connecting The SuperCollider to Erlang.

Having connected the Sonic Pi to Erlang I started idly wondering
``How does the Sonic Pi actually make sounds? does it do it itself
or does in use some other code to do this'' - I downloaded the
sources to find out - the documentation doesn't say.

Low and behold the Sonic Pi makes sounds by sending OSC-over-UDP
messages to the SuperCollider. I say the SuperCollider here, since
this is the name of the program you have to download to perform these
experiments. But actually the SuperCollider itself is actually two
programs, which communicate by (guess what) OSC-over-UDP.

A program called `scsynth` which is part of the SuperCollider program
generates the actual sounds. ``Why is it done this way?'' you're probably asking.
It's because the author of the SuperCollider wanted to separate the program
into a real-time part (`scsynth`) and a control part (which has less stringent control
problems) - the music should play (via `scsynth`) even if the controller is
busy or crashed for a short time - a very nice design.

Sam kindly told me how to trace the OSC messages to the SuperCollider
and I turned this into an Erlang program.

So to control the SuperCollider I open UDP port 4556 and send it a
stream of OSC messages - ie I'm doing exactly what the Sonic Pi did,
only I'm doing it from Erlang and without the user interface.

The code is in
[sc.erl](https://github.com/joearms/music_experiments/blob/master/sc.erl)
All I've done is start a `scsynth` server listening to port 4556
(this is done in
[start_scsynth.sh](https://github.com/joearms/music_experiments/blob/master/start_scsynth.sh)) and send it a load of OSC commands.

Working in the SuperCollider was an eye opener. It a REPL *without* a terminal
and is similar to the Plan9
[ACME](https://en.wikipedia.org/wiki/Acme_%28text_editor%29) shell/editor.

I'd read about ACME but never experienced it.  It's a Visceral
experience. **You never type code twice**. If it's on the screen you
click it, the answer appears ``somewhere else''.

Interestingly `Pure data` makes exactly the same design decision.

Why why why? Because it's faster to interact with. No typing just
clicking - this is important for live performance.  Musicians
instruments are not laptops with keyboards - they are things you can
hit and stroke.

# Act 3 - Pure Data

Finally [Pure Data](https://puredata.info/). PD programs have no
textual representation, there is only a graphic interface to a data-flow language.	

The program is in https://github.com/joearms/music_experiments/blob/master/pd_osc.pd:

<img src="/images/pd_osc.png">

The Erlang code
[pd.erl](https://github.com/joearms/music_experiments/blob/master/pd.erl)
is extremely simple:

    play(N) -> run_code(["/playNote", N]).

    run_code(M) ->
        E = osc:encode(M),
        {ok, Socket} = gen_udp:open(0,[binary]),
        ok = gen_udp:send(Socket, "localhost", 6677, E),
        gen_udp:close(Socket).



