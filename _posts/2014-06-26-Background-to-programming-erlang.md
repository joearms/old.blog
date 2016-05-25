---
layout: post
title: Background to Programming Erlang
tags: erlang 
year: 2013
month: 06
day: 25
published: true
summary: Background to Programming Erlang
---

Erlang is now over 25 years old. I've been involved with Erlang from
the very start, and seen it grow from a idea into a fully fledged
programming language with large numbers of users.

I wrote the first Erlang compiler, taught the first Erlang course, and
with my colleagues wrote the first Erlang book.  I started one of the
first successful Erlang companies and have been involved with all
stages of development of the language and its applications.
 
In 2007 I wrote ``Programming Erlang'' (Pragmatic Press) - it had been
14 years since the publication of ``Concurrent Programming in Erlang''
(Prentice Hall, 1993) - and our users were crying out for a new book.
So in 2007 I grit my teeth and started writing. I had the good fortune
to have Dave Thomas as my Editor and he taught me a lot about writing.
The first edition was pretty ambitious, I wanted to describe every
part of the language and the major libraries, with example code and
with real world examples that actually did things. So the book
contained runnable code for things like a SHOUTcast server so you
could stream music to devices and a full text indexing system.

The first edition of ``Programming Erlang'' spurred a flurry of
activity, the book sold well. It was published through the Pragmatic
Press Beta publishing process. The beta publishing process is great,
authors get immediate feedback from their readers. The readers can
download a PDF of the unfinished book and start reading and commenting
on the text.  Since the book is deliberately unfinished they can
influence the remainder of the book.  Books are published as betas
when they are about 70% complete.
 
On day one over 800 people bought the book, and on day two there were about
a thousand comments in the errata page of the book. How could there be
so many errors? My five hundred page book seemed to have about 4
comments per page. This came as a total shock. Dave and I slaved away,
fixing the errata.  If I'd known I'd have taken a two week holiday when
the book went live.

A couple of months after the initial PDF version of the book, the final
version was ready and we started shipping the paper version.

Now a strange thing happened - The Prags had published a Erlang book
and word on the street was that it was selling well. In no short order
I began hearing rumors, O'Reilly was on the prowl looking for authors,
many of my friends were contacted to see if they were interested in
writing Erlang books.

This is really weird, when you want to write a book you can't find a
publisher. But when an established publisher wants to publish a book
on a particular topic it can't find authors.

Here's the time line since 2007	

* 2007 - Programming Erlang - Armstrong - (Prag Press) 
* 2009 - ERLANG Programming - Cesarini and Thompson - (O'Reilly) 
* 2010 - Erlang and OTP in Action Logan - Merritt and Carlsson - (Manning) 
* 2013 - Learn You Some Erlang for Great Good - Hebert - (No starch press) 
* 2013 - Introducing Erlang: Getting Started in Functional Programming - St. Laurent (O'Reilly)
* 2013 - Programming Erlang - 2'nd edition - Armstrong - (Prag Press) 
* 2014 - Designing for Scalability with Erlang/OTP - Cesarini and Vinoski - (O'Reilly)

Not only was Erlang getting some love, languages like Haskell needed
to compete so Real World Haskell by Bryan O'Sullivan, John Goerzen and
Don Stewart was published in 2008.  This was followed by ``Learn You a
Haskell for Great Good'' by Miran Lipovaca (2011).

My Erlang book seemed to break the ice, O'Reilly followed with
``Erlang Programming'' and ``Real World Haskell'' - which inspired
Starch press and ``Learn You a Haskell for Great Good'' which inspired
``Learn You Some Erlang for Great Good'' and the wheel started to
spin.

Fast Forward to 2013
====================

I was contacted by the Prags ``did my book want a refresh?'' -
what's a refresh?  The 2007 book was getting a little dated. Core
Erlang had changed a bit, but the libraries had changed, and the user
base had grown.  But also, and significantly for the second edition
there were now four other books on the market.

My goal in the first edition had been ``describe everything'' and
``document everything that is undocumented''. I wanted a book that was
complete in its coverage and I wanted a book for beginners and for
advanced users.

Now of course this is impossible. A book for beginners will have a lot
of explanations that the advanced user will not want to read. Text for
advanced users will be difficult for beginners to understand, or
worse, impossible to understand.

When I started work on a second edition - I thought - ``All I'll have to
do is piff up the examples and make sure everything works properly.'' I
also thought I'd drop some rather boring appendices, drop the odd
chapter and add a new chapter on the type system - so I thought.

It didn't turn out like this, My editor, the ever helpful Susannah
Pfalzer, probably knew this, but wasn't letting on.

In the event I wrote seven new chapters, dropped some rather boring
appendices and dropped some old chapters.

The biggest difference in the second edition was a refocusing the target
audience.  Remember I said that the first edition was intended for
advanced and beginning users.  Well now there were four competing books
on the market. Fred Hebert's book was doing a great job for the
first-time users, with beautifully hand draw cartoons to
boot. Francesco and Simons book was doing a great job with OTP, so now
I could refocus the book and concentrate on a narrow band of users.

But who? In writing the second edition we spent a lot of time focusing on
our target audience. When the first seven chapters were ready we sent
the chapters for review to 14 reviewers. There were three super advanced
users - the programmers who know __everything__ about Erlang. What they don't
know about Erlang could be engraved on the back of a tea-leaf, we also took
four total beginners - who know how to program in Java but who
knew no Erlang, and the rest were total beginners, they'd been hacking
Erlang for a year or so but were still learning.

We threw the book at them to see what would happen.

Surprise number one:

Some of the true beginners didn't understand what I'd written - some
of the ideas were just ``too strange''. I was amazed - goodness gracious
me, when you've lived breathed dreamed and slept with Erlang for
twenty five years and you know Erlangs aunty and grandmother you take
things for granted.

And so I threw away the text that they didn't understand and
started again.  I'd got one reviewer on the ball, - I redid the text -
they read it again - they still didn't understand - it was very frustrating.

``what is wrong?'' I'm busting a gut explaining __everything__ and
they still don't understand. And so I threw the text away (again)
re-wrote it and sent them the third draft.

Bingo - they understood. Happy days are here again. Sometimes new
ideas are just ``too strange'' to grock. But by now I was getting a
feeling for how much explanation I had to add, it was about 30% more
that I thought - but what the heck, if you've bought a book you don't
want the people who've bought the damn thing to not read it because
it's too difficult.

I also had Bruce Tate advising me - Bruce wrote	 ``learn 600
languages in ten minutes flat.''  Bruce is great guy, and does a mean
Texas Accent if you feed him on beer and ask him nicely.  Now Bruce
can teach any programming language to anybody in ten seconds flat, so
he's a great guy to have reviewing your books.

What about the advanced users? My book was 30% longer with 30% more
text and was aimed at converting Java programmers who have seen the
light and are wish to renounce their evil ways and convert to the joys
of Erlang programming, but what about the advanced users?

Screw the advanced users - they know it all anyway - and they don't buy
the book because they know it all anyway. So I killed by babies, and
threw out a lot of advanced material that nobody ever reads - My goal
is to put the omitted advanced material on a web site.

The other tip I got was from Francesco Cesarini - ``They like the
exercises'' So I added exercises at the end of virtually every
chapter.

So now there is no excuse for not holding an Erlang programming course,
there are exercises at the end of every chapter.



 
  