---
layout: post
title: Solving the wrong problem
tags: parallelism concurrency sequential
year: 2013
month: 3
day: 27
published: true
---

We're right and the rest of the world is wrong. We (that is Erlang
folks) are solving the right problem, the rest of the world (non
Erlang people) are solving the wrong problem.

The problem that the rest of the world is solving is <i>how to
parallelise legacy code</i>. Up to about 2004 Moore's law
applied. Each year your programs just got faster, you didn't have to
be a better programmer, you didn't need a smarter algorithm your
machine just got faster year on year.

Chips got bigger and bigger, clock speeds got greater and greater,
and programs went faster and faster which improved performance by about
15% per year.

In 2004 this ended. The chips were so big and the clock rates so fast
that clock pulses could not reach all parts of the chip in one
clock cycle. Circuit designs changed. The multi-core came.

From 2004 chips still got bigger, but clock rates started sinking and
the number of CPUs per chip started increasing. We moved from the era
of one superfast processor per chip, to several slower and weaker
processors per chip.

At this point in time, sequential programs started getting slower, year on year,
and parallel programs started getting faster.

The problem was that there were no parallel programs, or at least very few.

Now Erlang is (in case you missed it) a concurrent language, so Erlang
programs should in principle go a lot faster when run on parallel
computers, the only thing that stops this is if the Erlang programs
have sequential bottlenecks.

Amdahl's law hits you in the face if your parallel program has any sequential parts.

Suppose 10% of your program is sequential (the rest being parallel) -
the time to execute the parallel bit can be shrunk to zero by having
sufficiently many parallel processors. But the sequential part will
remain.

With 10% sequential code the maximum speedup of your program will be a
factor 10. One tenth of the program can never speed up, the time for
the other 9/10's can shrink to zero.

So for Erlang programmers the trick to speeding up their programs is
to find the sequential parts in the code.

For anybody who writes sequential code the trick to speeding up their
programs is to find the parallel parts in their code.

The road to automatic parallelisation of sequential programs is
littered with corpses.  It can't be done. (not quite true, in some
specific circumstances it can, but this is by no means easy).

So now data centers are being filled with shiny new computers
and the top-end machines have as many as 24 cores. But what about performance?
Are these shiny new machines going 24 times faster?

For some problems yes - but for many problems no. For many problems
only one of the 24 CPUs is being used. The underutilization of the CPUs is
a serious problem.  This point was pointed out in Alexander Gounares
Brilliant talk at the Erlang factory.

Alexander's talk gave us a glimpse of the future. His company <a
href="http://concurix.com/">concurix</a> is showing us where the
future leads.  They have tools to automate the detection of sequential
bottlenecks in Erlang code.

Concurix have been using these tools to find bottlenecks in the Erlang
VM and in their test code and the results are amazing.  They found a
bottleneck in an image processing application, there was a lock in zlib
which was written in C. They rewrote it in Erlang, going from C to Erlang.

This is crazy, C should go faster, well yes it does but it had a
lock. Erlang was slower but lock-free and thus scaleable. So removing the C
and doing image processing in Erlang was faster than doing it in C.

I was amazed - this is jaw dropping good stuff.

When the videos from the Erlang factory come out watch Alexander's
talk and prepare to be amazed. The future is here it arrived last week
in San Francisco.
 
  




