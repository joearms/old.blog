---
layout: post
title: Testing testing 123
tags: jekyll bootstrap github disqus
year: 2013
month: 3
day: 26
published: true
summary: testing my blog
---

This is a test of Jekyll. If it works I'll move all my blogs to github.

ToDo
====

* <strike>Fix RSS feeds</strike>

RSS
===

I have set up an RSS atom feed. Hope it works


Reference Material
==================

* http://cobyism.com/jekyll/docs/home/
* http://paulstamatiou.com/how-to-wordpress-to-jekyll
* http://spparker.com/posts/2011-04-26-keeping-jekyll-classy
* http://help.github.com/articles/github-flavored-markdown.html

Code highlighting test
======================

Can we highlight Erlang code?

{% highlight erlang %}
start([Config]) ->
    File = (atom_to_list(Config)),
    io:format("ws start:~p~n",[File]),
    Conf = read_config(File),
    start_link(Conf).
{% endhighlight %}

Yes!


Quotes test
===========

Quotes seem to work properly. I can make &ldquo;curly quotes&rdquo; and
``regular quotes''.

Stuff that is broken
====================

* Colored quoting of text
* My tweet button is wrong

Making Autolinking work
==========

To get autolinking to work needs some changes to <b>_config.yml</b>

    rdiscount:
        extensions: [smart, autolink]




Comments
========

I have used Disqus for this. It seems to work nicely.
If anybody knows how to make a tweet link or why URLs are not autolinked please
add a comment.

Tests
=====

Page.title = ({{page.title}})
Page.url = ({{page.url}})







