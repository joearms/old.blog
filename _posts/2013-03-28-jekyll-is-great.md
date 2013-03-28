---
layout: post
title: Jekyll is great
tags: jekyll blogging
year: 2013
month: 3
day: 28
published: true
---

It's now two days since my new blog first appeared. Actually I hadn't
even tweeted or announced it in any way. I just wanted to setup things
<b>before</b> I went public.

To my amazement the twitterati noticed and my blog was being read
before I'd even figured out how the templates work and how to even
write a blog.

This is the first blogging system that I've ever used where an
overwhelming desire to rewrite the entire blogging engine did not come
over me as soon as I got started.

My life as a failed blogger ...
========================

* Start blogging ...
* Writing the blog is fun, but, ...
* Entering the blog in the content management system is painful, so, ...
* I think ``I'll implement my own blogging engine'', and, ...
* I start concentrating on how to implement a blogging engine, and, ...
* I Forget to write any content, so, ...
* Blog dries up - no content.

My experiences with Google's blog engine were not very encouraging.
My blog at http://armstrongonsoftware.blogspot.com/ was done with a
content management system. A complete pain-in-the-arse to use. I don't
want a dashboard, and above all I don't want a WYSIWYG editor that can
nip backwards and forwards between WYSIWYG mode and editing the raw
underlying HTML.  This sucks big time.

The String Problem
===================

All Programming languages I know of suffer from ``the string problem''.
String literals in Erlang are written like this:


{% highlight erlang %}
start() ->
    "Hello world".
{% endhighlight %}

Which is crazy. The start and stop symbol is the same (ie a single quote mark)
It would be far far better to use different symbols for the start and stop symbols.

We should write <b>``Hello world''</b> using different symbols for the start and stop quotes.

This would give us far more precise error messages. Sometimes if you
miss out a closing quote the rest of the program up to the next
quote gets swallowed up as a string literal and seeing exactly where
this happened can be painful. The same bug is in all programming
language that I am aware of, so we are not alone.

English uses ``curly quotes'' for string literals, and the glyphs themselves for
the open and close quotes are rather beautiful. Magnify the glyphs in a nice typeface,
(say Garamond) and you'll see what I mean.

This is not my first rant about quotes, I told Google off about this in
http://armstrongonsoftware.blogspot.com/2012/06/what-happened-to-my-quotes.html
but they still haven't fixed the bug. Gmail still gets typographic
quotes wrong, so I feel ill every time I have to type quotes into
gmail.

<b>If anybody who reads this blog works at Google please fix this.</b>

I've downloaded dozens of Javascript editors, and they all get typographic quotes wrong.

``getting your quotes right,'' Said George, ``is important.''

``Why,'' asked Margery.

``When a man is tired of correctly quoting his strings, he is tired of life,'' said George.

``I thought it was London,'' said Margery.

I can't use a blogging system that gets the simplest form of
typography wrong.  If Gutenberg could get quotes right, and he had to
invent printing to do so then we should be able to do so.

We have our bright shiny computers with their GHz clocks, and they
can't even get quotes right.

Jekyll (or at least the markdown thing) uses LaTeX style quotes
which makes an unsad happy bunny.

I'm an emacs man 
================

Type in the text in Emacs, run the result through the
compiler/formatter/whatever.  Stare at the results. Wash and repeat,
tumble dry, iron the crumply bits and you're done.

OK now thanks the Jekyll I can type my text in emacs, run the text
through a spell checker (and If I have time through a speech
synthesizer, this finds many extra errors, since I tend to read what I
think I've written and not what I've actually written) type Make and
stare at the result and if it's OK, a quick <b>git push</b> and Bob's
your auntie.

And then an amazing thing happened...
==================================

I got pull requests.   


Sylvain Benner has sent my two pull request to fix my spelling
errors. Thank you Sylvian. I'm not the world's best speller. So I'm
pathetically gratefully for all the help I can get. So thank you once
again.
