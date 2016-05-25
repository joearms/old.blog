---
layout: post
title: Fail Fast and Noisily, Fail Politely
tags: error messages politeness
year: 2013
month: 4
day: 23
published: true
---

In [Programming Erlang 2'nd edition](http://pragprog.com/book/jaerlang2/programming-erlang)
I described a new design principle.

When applications fail, the programmer should provide **two** error
messages.  Not one. The first error message, the ``noisy'' message is
intended for the programmer. It should provide a heck of a lot of
information, which should be put into a permanent log file, and from
which it should be possible to do a post-mortem debugging of the
program to see what went wrong.

The second error message should obey the ``fail politely'' rule.  A
polite error message should be shown to the poor unsuspecting user of
the program, preferably with a grovelling apology for wasting their
time.

If a web site powered by PHP fails to work because it can't connect
to a SQL database I don't want to know - really, I don't want to
know, I am totally uninterested in this. **TOTALLY**, sorry for shouting,
I want you to imagine me typing this in on my keyboard or rather,
banging in the text with rather vigorous keystrokes. **I JUST DON'T
WANT TO KNOW WHY YOUR PROGRAM FAILED.**


What do you want me to do with these awful error messages, print them
out and put them in a bottle and drop it into the nearest sewer? I
haven't a clue.


Please send good and bad examples of error messages
===================================================

I'd really like to see some __really bad__ error messages, and some good ones
so feel free to post comments to this blog. 

I'm sure that the programmer who wrote the code that crashed wants
to know why their program crashed which is why all this precious
information about why the program crashed should be stored in a file
or a database or in the cloud FOREVER and not just scroll past the
screen, and even worse be sent to the poor user.

Some error messages are positively mind-bogglingly awful, hundreds of
lines of meaningless crap. I've even seen error messages which refer to
error codes that don't exist.

I won't name names here, so I won't tell you who but a certain company
that makes software that unfortunately it has been my duty to use,
crashed with 3 different numerical error codes. Numerical error codes,
I ask you, NUMERICAL, (Sorry I'm shouting again) - the topic of whether
to have numerical codes or human readable text was a hot topic in the
late 1970's so I was somewhat surprise to see numerical error codes
still in existence.

After a bit of Googling I found a web site claiming to have the big
list of all error codes that this program was known to have
produced. Unfortunately only two of the codes were in the bug list -
and worse I couldn't understand the explanation of the error codes
that were in the list. The third error code didn't exist.

The same program, had a link to some help desk with some fatuous text
saying they welcomed comments about their program.
When I mailed this address describing in detail what they should do
with their software, I got a polite reply, saying my comments had been
noted and passed on to their development group on one of the moons
of Titan.

Sign the pledge
============

I ..... (name of programmer) promise to never let horrible error
messages out of my program where they can offend the sensibilities of
the poor sods who have to use my program.

Print the pledge out and glue it to your computer.

