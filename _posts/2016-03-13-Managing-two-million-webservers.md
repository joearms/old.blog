---
layout: post
title: Managing two million web servers
tags: erlang elixir
year: 2016
month: 3
day: 13
published: true
---

This has been bugging me for a long time. There are some things we
explain because we know other people don't understand them and there
are some things we don't explain because we assume that
everybody else knows them.

So this is about web-servers written in Erlang and Elixir.  The idea
to write this came after watching the [Making the Web
Functional](https://www.youtube.com/watch?v=XJ9ckqCMiKk) presentation
by Chris McCord and Evan Czaplicki at the [2016 Erlang Factory in San
Francisco](http://www.erlang-factory.com/sfbay2016).

Imagine an Erlang or Elixir HTTP server managing a couple of million
user sessions.  Time and again I've heard this said:

+ We have a (Erlang or Elixir) web server managing 2 million user sessions.

But this statement is incorrect and stems from a fundamental misconception.

> **We do not have ONE web-server handling 2 millions sessions. We have
2 million webservers handling one session each.**

The reason people think we have one webserver handling a couple of
million users is because this is the way it works in a sequential web
server. A server like Apache is actually a single webserver that
handles a few million connections.

In Erlang we create very lightweight processes, one per connection and
within that process spin up a web server. So we might end up with a
few million web-servers with one user each.

If we can accept say 20K requests/second - this is equivalent to
saying we can create 20K webservers/second.

On the surface things look very similar. But there is a fundamental
difference between having one webserver handling two million
connections, and two million web servers handling one connection each.

If there is a software error and the server software crashes we lose
either two million connections or one depending upon the model.

In Erlang if the web server software itself is incorrect we'll lose a
single connection, which is OK. Since the software is incorrect and
crashes we don't know what to do so crashing is a good
alternative. What is important is that one session crashing does not
effect all the other sessions.

This requirement, goes way back to when we designed Erlang in the mid
1980's.  In Telecoms systems, losing one connection due to a software
error was acceptable, losing them all due to a bug was big time bad
news.

# Chat or Presence Servers

So what happens if you want to make a chat or presence server? Well
we've already got one process per connection, so let's also make one
process per user and send messages between the connections and user
processes when connections come and go - this is so simple that the
code practically writes itself.

So why did WhatsApp use Erlang and why does the
[Phoenix Framework](http://www.phoenixframework.org/)
outperform Ruby on Rails? - precisely because we have millions of tiny
webservers - and when we have lots of small things it's easy to spread
them over lots of processors and make the system fault-tolerant and scalable.

Packing Erlang or Elixir processes onto cores is easy becuase they are
small and are like packing physical objects. If we want to pack sand
in barrels it's easy. The grains of sand are so small that it's easy
to completely fill the barrels. Packing huge boulders is difficult,
they don't pack well and much space is wasted.

# Adding fault-tolerance and scalability

> Because we have one web server per user  we can easily make the
system fault tolerant or scalable

To make a fault tolerent system we use two or more processes per user;
One is the master process, the others are replicas on different
machines. They must be on different machines since the entire machine
where the master runs might crash. We can make it scalable  by just
buying more machines and spreading the processes out over the
machines.
 












  

