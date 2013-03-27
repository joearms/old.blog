---
layout: post
title: Testing testing 123
---

{{ page.title }}
================

<p class="meta">26 March 2010</p>

This is a test of Jekyll. If it works I'll move all my blocks to github.

The is a <a href="http://cobyism.com/jekyll/docs/home/">guide</a> and
<a href="http://paulstamatiou.com/how-to-wordpress-to-jekyll">How to
wordpress to Jekyll</a> was also a good read.

Code highlighting test
======================

{% highlight erlang %}
start([Config]) ->
    File = (atom_to_list(Config)),
    io:format("ws start:~p~n",[File]),
    Conf = read_config(File),
    start_link(Conf).
{% endhighlight %}

Quotes
======

Quotes don't work properly. I want to see &ldquo;curly quotes&rdquo; and
not ``regualr quotes''. How can markdown (or whatever this is) get this wrong?


Stuff that is broken
====================

Colored quoting of text.

References
==========

https://help.github.com/articles/github-flavored-markdown

Comments
========

Disqus seems ok for this. This needs to be setup








