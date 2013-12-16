---
layout: post
title: My favorite Erlang program
tags: erlang
year: 2013
month: 11
day: 21
published: true
---

The other day I got a mail from Dean Galvin from Rowan University.
Dean was doing an Erlang project so he asked ``What example program
would best exemplify Erlang''.

He wanted a small program, that would be suitable for a ten minute
talk that would best show off the language.  I thought for a while
... and quickly wrote my favorite program, it's the ``Universal
server''.

The Universal Server
====================

Normally servers do something. An HTTP server responds to HTTP
requests and FTP server responds to FTP request and so on. But what
about a __Universal Server__? surely we can generalize the idea of a
server and make a universal server that which we can later tell to
become a specific sever.

Here's my universal server:

{% highlight erlang %}
universal_server() ->
    receive
       {become, F} ->
           F()
    end.
{% endhighlight %}

That was pretty easy. Once I have created a universal serve it just sits and
waits for a **{become, F}** message and then it becomes an **F** server.

The Factorial Server
====================

A factorial server is a server which waits for an integer and sends back the
factorial of an integer. This is mind-boggling simple:

{% highlight erlang %}
factorial_server() ->
    receive
       {From ! N} ->
           From ! factorial(N),
           factorial_server()
    end.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
{% endhighlight %}

Now we're ready to rock and roll ...

Putting it all together
======================

I'll write a little function that creates a universal server
sends it a ``become a factorial server'' message, then I'll sent 
it an integer, wait for the response and print the response:


{% highlight erlang %}
test() ->
    Pid = spawn(fun universal_server/0),
    Pid ! {become, fun factorial_server/0},
    Pid ! {self(), 50},
    receive
        X -> X
    end.
{% endhighlight %}
 
All these functions belong to the module **demo**.

Now all we have to do is fire up an Erlang shell and run the test program

{% highlight erlang %}
$ erl
1 > c(demo1).
{ok, demo1}
2 > demo1:test().
30414093201713378043612608166064768844377641568960512000000000000
{% endhighlight %}

Aside
=====

A few years ago when I was at SICS I had access to [Planet
Lab](http://www.planet-lab.org/).  Planet Lab is a research network of
9000 computers. Joining Planet Lab is easy, all you have to do is buy
a standard PC, connect it to the network and donate it's use to the
Planet Lab organization. Having donated you machine to the network, in
return you can use all the other machines in planet lab.

Planet lab is a real-world test-bed for distributed applications, it
currently has 1171 nodes at 562 sites.

What was I going to do with planet lab? I didn't have a clue.  What I
ended up doing was making some scripts to install empty universal
Erlang servers on all the Planet lab machines (pretty much like the
code in this article) - then I set up a gossip algorithm to flood the
network with **become** messages. Then I had an empty network that in a
few seconds would become anything I wanted it to do.

About a year later I had to write a paper. One of the disadvantages of
being a researcher is that in order to get money you have to write a
paper about something or other, the paper is never about what
currently interests you at the moment, but must be about what the
project that financed your research expects to read about.

Well I had my gossip network setup on planet lab and I could tell it
to become anything, so I told it to become a content distribution
networks and used a gossip algorithm to make copies of the same file
on all machine on the network and wrote a paper about it and everybody
was happy.



